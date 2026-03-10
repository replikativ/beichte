(ns beichte.analyze
  "Effect inference via AST walking.

  Performs lightweight abstract interpretation over the effect lattice:
  walks the tools.analyzer.jvm AST and infers the effect level and
  feature flags of each node by propagating the join through the call
  graph.

  The analysis is modular: it uses an immutable registry for ground
  truth and maintains a mutable cache for recursive var analysis.
  The cache is per-analysis-session, not global.

  Two inference modes:
  - `infer-effect`  — returns bare effect level (:pure, :local, etc.)
  - `infer-descriptor` — returns full descriptor {:effect :level :flags #{...}}"
  (:require [beichte.effect :as effect]
            [beichte.registry :as registry]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.jvm :as jvm]
            [clojure.java.io :as io])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT)))

;; ================================================================
;; Source retrieval
;; ================================================================

(defn source-var
  "Retrieve the source code string for a var from classpath or filesystem.
  Returns the source string, or nil if unavailable."
  [v]
  (when-let [filepath (:file (meta v))]
    (when-let [strm (or (.getResourceAsStream (RT/baseLoader) filepath)
                        (try (io/input-stream filepath) (catch Exception _ nil)))]
      (try
        (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
          (dotimes [_ (dec (or (:line (meta v)) 1))] (.readLine rdr))
          (let [text (StringBuilder.)
                pbr (proxy [PushbackReader] [rdr]
                      (read [] (let [i (proxy-super read)]
                                 (.append text (char i))
                                 i)))
                read-opts (if (.endsWith ^String filepath "cljc")
                            {:read-cond :allow} {})]
            (read read-opts (PushbackReader. pbr))
            (str text)))
        (catch Exception _ nil)))))

;; ================================================================
;; Known random/throwing operations
;; ================================================================

(def ^:private random-vars
  "Vars known to use randomness."
  #{#'clojure.core/rand #'clojure.core/rand-int #'clojure.core/rand-nth
    #'clojure.core/shuffle})

(def ^:private random-methods
  "Java methods known to use randomness."
  #{["java.util.UUID" 'randomUUID]
    ["java.lang.Math" 'random]
    ["java.util.concurrent.ThreadLocalRandom" 'current]})

;; ================================================================
;; Analysis state
;; ================================================================

(defn make-analysis
  "Create a fresh analysis state.
  - registry: immutable map of known effects (level or descriptor)
  - cache: atom of {entry -> effect-level}, populated during analysis"
  ([registry]
   {:registry registry
    :cache (atom {})})
  ([]
   (make-analysis (registry/default-registry))))

(defn- cached-effect
  "Look up an entry's effect level, checking cache first, then registry.
  Registry may store bare levels or descriptors — always returns a level or nil."
  [{:keys [registry cache]} entry]
  (let [cached (get @cache entry)]
    (if cached
      (if (map? cached) (:effect cached) cached)
      (let [reg-val (registry/lookup registry entry)]
        (if (map? reg-val) (:effect reg-val) reg-val)))))

(defn- cached-descriptor
  "Look up an entry's full effect descriptor (with flags)."
  [{:keys [registry cache]} entry]
  (let [cached (get @cache entry)]
    (cond
      (nil? cached)
      (let [reg-val (registry/lookup registry entry)]
        (cond
          (nil? reg-val)     nil
          (map? reg-val)     reg-val
          :else              (effect/effect reg-val)))
      (map? cached) cached
      :else         (effect/effect cached))))

(defn- cache-effect!
  "Cache an analysis result (level or descriptor)."
  [{:keys [cache]} entry val]
  (swap! cache assoc entry val)
  val)

;; ================================================================
;; AST effect inference (level-only, fast path)
;; ================================================================

(declare infer-effect)

(defn- infer-effects-join
  "Infer effects for a collection of AST nodes and join them."
  [analysis visited nodes]
  (reduce (fn [acc node]
            (if node
              (effect/join acc (infer-effect analysis visited node))
              acc))
          :pure
          nodes))

(defn- infer-var-effect
  "Infer the effect of a var by analyzing its source.
  Caches the result. Uses visited set for cycle detection."
  [analysis visited ast]
  (let [v (or (:var ast) (:the-var ast))]
    (if-let [known (cached-effect analysis v)]
      known
      ;; Not known — try to analyze source
      (let [src (source-var v)]
        (if (nil? src)
          (cache-effect! analysis v :io)
          (let [analyzed (try
                           (-> src
                               read-string
                               (jvm/analyze (assoc (jvm/empty-env)
                                                   :ns (symbol (str (:ns (meta v)))))))
                           (catch Exception _ nil))]
            (if analyzed
              (let [eff (infer-effect analysis (conj visited v) analyzed)]
                (cache-effect! analysis v eff))
              (cache-effect! analysis v :io))))))))

(defn infer-effect
  "Infer the effect level of an AST node.
  Returns one of: :pure, :local, :mutation, :io"
  ([analysis ast]
   (infer-effect analysis #{} ast))
  ([analysis visited ast]
   (if (nil? ast)
     :pure
     (let [{:keys [args op]} ast]
       (case op
         nil :pure

         :static-call
         (let [class-str (pr-str (:class ast))
               method (:method ast)
               key [class-str method]
               known (cached-effect analysis key)]
           (if known
             (effect/join known (infer-effects-join analysis visited args))
             :io))

         :const
         (case (:type ast)
           :class (let [class-str (pr-str (:form ast))
                        known (cached-effect analysis class-str)]
                    (if (nil? known) :io (or known :pure)))
           (:vector :map :set :nil :number :keyword :string :bool :constant :var :type :record :seq :fn :char :regex :unknown) :pure)

         :def
         (infer-effect analysis (conj visited (:var ast))
                       (or (-> ast :init :expr) (-> ast :init)))

         :fn (infer-effects-join analysis visited (:methods ast))
         :fn-method (infer-effect analysis visited (:body ast))

         :do (effect/join
              (infer-effects-join analysis visited (:statements ast))
              (infer-effect analysis visited (:ret ast)))

         :local (if (:assignable? ast) :local :pure)

         :invoke (effect/join
                  (infer-effect analysis visited (:fn ast))
                  (infer-effects-join analysis visited args))

         :try (effect/join-all
               [(infer-effect analysis visited (:body ast))
                (infer-effects-join analysis visited (:catches ast))
                (infer-effect analysis visited (:finally ast))])

         :catch (infer-effect analysis visited (:body ast))
         :instance? :pure

         :let (effect/join
               (infer-effects-join analysis visited (:bindings ast))
               (infer-effect analysis visited (:body ast)))

         :binding (infer-effect analysis visited (:init ast))

         :instance-call
         (let [class-str (pr-str (:class ast))
               method (:method ast)
               key [class-str method]
               known (cached-effect analysis key)]
           (if known
             (effect/join known (infer-effects-join analysis visited args))
             :io))

         :loop (effect/join
                (infer-effects-join analysis visited (:bindings ast))
                (infer-effect analysis visited (:body ast)))
         :recur (infer-effects-join analysis visited (:exprs ast))

         :new (let [class-eff (infer-effect analysis visited (:class ast))]
                (effect/join class-eff
                             (infer-effects-join analysis visited args)))

         :if (effect/join-all
              [(infer-effect analysis visited (:test ast))
               (infer-effect analysis visited (:then ast))
               (infer-effect analysis visited (:else ast))])

         :vector (infer-effects-join analysis visited (:items ast))
         :set (infer-effects-join analysis visited (:items ast))
         :map (effect/join
               (infer-effects-join analysis visited (:keys ast))
               (infer-effects-join analysis visited (:vals ast)))

         (:var :the-var)
         (if (contains? visited (:var ast))
           :pure
           (infer-var-effect analysis visited ast))

         :static-field :pure
         :instance-field :mutation

         :with-meta (effect/join
                     (infer-effect analysis visited (:expr ast))
                     (infer-effect analysis visited (:meta ast)))

         :host-interop (infer-effects-join analysis visited args)
         :keyword-invoke (infer-effects-join analysis visited args)
         :protocol-invoke (infer-effects-join analysis visited args)
         :prim-invoke (infer-effects-join analysis visited args)

         :monitor-enter :mutation
         :monitor-exit :mutation

         :throw (infer-effect analysis visited (:exception ast))

         :set! :mutation

         :case (effect/join-all
                [(infer-effect analysis visited (:test ast))
                 (infer-effects-join analysis visited (:thens ast))
                 (infer-effect analysis visited (:default ast))])

         :letfn (effect/join
                 (infer-effects-join analysis visited (:bindings ast))
                 (infer-effect analysis visited (:body ast)))

         :method (infer-effect analysis visited (:body ast))

         :io)))))

;; ================================================================
;; AST descriptor inference (with flags)
;; ================================================================

(declare infer-descriptor)

(defn- infer-descriptors-join
  "Infer descriptors for a collection and join them."
  [analysis visited nodes]
  (reduce (fn [acc node]
            (if node
              (effect/effect-join acc (infer-descriptor analysis visited node))
              acc))
          effect/pure
          nodes))

(defn- infer-var-descriptor
  "Infer the full descriptor of a var (with flags)."
  [analysis visited ast]
  (let [v (or (:var ast) (:the-var ast))
        ;; Check for known flags
        base-flags (cond-> #{}
                     (contains? random-vars v) (conj :random))]
    (if-let [known (cached-descriptor analysis v)]
      (update known :flags into base-flags)
      (let [src (source-var v)]
        (if (nil? src)
          (let [desc (effect/effect :io base-flags)]
            (cache-effect! analysis v desc)
            desc)
          (let [analyzed (try
                           (-> src
                               read-string
                               (jvm/analyze (assoc (jvm/empty-env)
                                                   :ns (symbol (str (:ns (meta v)))))))
                           (catch Exception _ nil))]
            (if analyzed
              (let [desc (infer-descriptor analysis (conj visited v) analyzed)
                    desc (update desc :flags into base-flags)]
                (cache-effect! analysis v desc)
                desc)
              (let [desc (effect/effect :io base-flags)]
                (cache-effect! analysis v desc)
                desc))))))))

(defn infer-descriptor
  "Infer the full effect descriptor of an AST node.
  Returns {:effect :level, :flags #{...}}"
  ([analysis ast]
   (infer-descriptor analysis #{} ast))
  ([analysis visited ast]
   (if (nil? ast)
     effect/pure
     (let [{:keys [args op]} ast]
       (case op
         nil effect/pure

         :static-call
         (let [class-str (pr-str (:class ast))
               method (:method ast)
               key [class-str method]
               known-desc (cached-descriptor analysis key)
               rand? (contains? random-methods key)
               method-desc (cond
                             known-desc (if rand?
                                          (update known-desc :flags conj :random)
                                          known-desc)
                             rand?      (effect/effect :pure #{:random})
                             :else      (effect/effect :io))]
           (effect/effect-join method-desc
                               (infer-descriptors-join analysis visited args)))

         :const
         (case (:type ast)
           :class (let [class-str (pr-str (:form ast))
                        known (cached-effect analysis class-str)]
                    (if (nil? known) (effect/effect :io) (effect/effect (or known :pure))))
           (:vector :map :set :nil :number :keyword :string :bool :constant :var :type :record :seq :fn :char :regex :unknown) effect/pure)

         :def
         (infer-descriptor analysis (conj visited (:var ast))
                           (or (-> ast :init :expr) (-> ast :init)))

         :fn (infer-descriptors-join analysis visited (:methods ast))
         :fn-method (infer-descriptor analysis visited (:body ast))

         :do (effect/effect-join
              (infer-descriptors-join analysis visited (:statements ast))
              (infer-descriptor analysis visited (:ret ast)))

         :local (if (:assignable? ast)
                  (effect/effect :local)
                  effect/pure)

         :invoke (effect/effect-join
                  (infer-descriptor analysis visited (:fn ast))
                  (infer-descriptors-join analysis visited args))

         :try (effect/effect-join-all
               [(infer-descriptor analysis visited (:body ast))
                (infer-descriptors-join analysis visited (:catches ast))
                (infer-descriptor analysis visited (:finally ast))])

         :catch (infer-descriptor analysis visited (:body ast))
         :instance? effect/pure

         :let (effect/effect-join
               (infer-descriptors-join analysis visited (:bindings ast))
               (infer-descriptor analysis visited (:body ast)))

         :binding (infer-descriptor analysis visited (:init ast))

         :instance-call
         (let [class-str (pr-str (:class ast))
               method (:method ast)
               key [class-str method]
               known-desc (cached-descriptor analysis key)
               method-desc (or known-desc (effect/effect :io))]
           (effect/effect-join method-desc
                               (infer-descriptors-join analysis visited args)))

         :loop (effect/effect-join
                (infer-descriptors-join analysis visited (:bindings ast))
                (infer-descriptor analysis visited (:body ast)))
         :recur (infer-descriptors-join analysis visited (:exprs ast))

         :new (effect/effect-join
               (infer-descriptor analysis visited (:class ast))
               (infer-descriptors-join analysis visited args))

         :if (effect/effect-join-all
              [(infer-descriptor analysis visited (:test ast))
               (infer-descriptor analysis visited (:then ast))
               (infer-descriptor analysis visited (:else ast))])

         :vector (infer-descriptors-join analysis visited (:items ast))
         :set (infer-descriptors-join analysis visited (:items ast))
         :map (effect/effect-join
               (infer-descriptors-join analysis visited (:keys ast))
               (infer-descriptors-join analysis visited (:vals ast)))

         (:var :the-var)
         (if (contains? visited (:var ast))
           effect/pure
           (infer-var-descriptor analysis visited ast))

         :static-field effect/pure
         :instance-field (effect/effect :mutation)

         :with-meta (effect/effect-join
                     (infer-descriptor analysis visited (:expr ast))
                     (infer-descriptor analysis visited (:meta ast)))

         :host-interop (infer-descriptors-join analysis visited args)
         :keyword-invoke (infer-descriptors-join analysis visited args)
         :protocol-invoke (infer-descriptors-join analysis visited args)
         :prim-invoke (infer-descriptors-join analysis visited args)

         :monitor-enter (effect/effect :mutation)
         :monitor-exit (effect/effect :mutation)

         ;; throw — state effect comes from the exception expr,
         ;; but adds :throws flag
         :throw (update (infer-descriptor analysis visited (:exception ast))
                        :flags conj :throws)

         :set! (effect/effect :mutation)

         :case (effect/effect-join-all
                [(infer-descriptor analysis visited (:test ast))
                 (infer-descriptors-join analysis visited (:thens ast))
                 (infer-descriptor analysis visited (:default ast))])

         :letfn (effect/effect-join
                 (infer-descriptors-join analysis visited (:bindings ast))
                 (infer-descriptor analysis visited (:body ast)))

         :method (infer-descriptor analysis visited (:body ast))

         ;; Unknown op
         (effect/effect :io))))))

;; ================================================================
;; High-level API
;; ================================================================

(defn- make-ast [expr ns-str]
  (let [wrapped `(fn [] ~expr)]
    (jvm/analyze wrapped (assoc (jvm/empty-env) :ns (symbol ns-str)))))

(defn- resolve-analysis
  "Resolve an analysis context from opts. If :analysis is provided, use it.
  Otherwise create a fresh one (optionally from :registry)."
  [{:keys [analysis registry]}]
  (cond
    analysis analysis
    registry (make-analysis registry)
    :else    (make-analysis)))

(defn analyze-expr
  "Analyze an S-expression and return its effect level.
  For full descriptors with flags, use analyze-expr-full.

  opts may include:
    :analysis — reuse a context from make-analysis (for caching)
    :registry — custom registry (ignored if :analysis given)
    :ns       — namespace context (default: *ns*)"
  ([expr] (analyze-expr expr {}))
  ([expr opts]
   (let [ns-str (or (:ns opts) (str (clojure.core/ns-name *ns*)))
         analysis (resolve-analysis opts)
         ast (make-ast expr ns-str)]
     (infer-effect analysis ast))))

(defn analyze-expr-full
  "Analyze an S-expression and return its full effect descriptor.
  Returns {:effect :level, :flags #{...}}."
  ([expr] (analyze-expr-full expr {}))
  ([expr opts]
   (let [ns-str (or (:ns opts) (str (clojure.core/ns-name *ns*)))
         analysis (resolve-analysis opts)
         ast (make-ast expr ns-str)]
     (infer-descriptor analysis ast))))

(defn analyze-var
  "Analyze a var and return its effect level."
  ([v] (analyze-var v {}))
  ([v opts]
   (let [analysis (resolve-analysis opts)]
     (or (cached-effect analysis v)
         (infer-var-effect analysis #{} {:var v})))))

(defn analyze-var-full
  "Analyze a var and return its full effect descriptor."
  ([v] (analyze-var-full v {}))
  ([v opts]
   (let [analysis (resolve-analysis opts)]
     (or (cached-descriptor analysis v)
         (infer-var-descriptor analysis #{} {:var v})))))

(defn analyze-ns
  "Analyze all public vars in a namespace and return a map of {var -> effect}."
  ([ns-sym] (analyze-ns ns-sym {}))
  ([ns-sym opts]
   (require ns-sym)
   (let [analysis (resolve-analysis opts)
         publics (ns-publics ns-sym)]
     (into {}
           (map (fn [[sym v]]
                  [v (or (cached-effect analysis v)
                         (infer-var-effect analysis #{} {:var v}))]))
           publics))))

(defn analyze-ns-full
  "Analyze all public vars and return {var -> descriptor}."
  ([ns-sym] (analyze-ns-full ns-sym {}))
  ([ns-sym opts]
   (require ns-sym)
   (let [analysis (resolve-analysis opts)
         publics (ns-publics ns-sym)]
     (into {}
           (map (fn [[sym v]]
                  [v (or (cached-descriptor analysis v)
                         (infer-var-descriptor analysis #{} {:var v}))]))
           publics))))

(defn compilable?
  "Check if an expression is within the effect budget for a compilation target.
  Uses full descriptor analysis (checks both state effects and feature flags)."
  ([target expr] (compilable? target expr {}))
  ([target expr opts]
   (let [desc (analyze-expr-full expr opts)]
     (effect/compilable? target desc))))

(defn filter-compilable
  "Filter a collection of vars to those compilable for a target."
  ([target vars] (filter-compilable target vars {}))
  ([target vars opts]
   (let [analysis (resolve-analysis opts)]
     (filterv (fn [v]
                (let [desc (or (cached-descriptor analysis v)
                               (infer-var-descriptor analysis #{} {:var v}))]
                  (effect/compilable? target desc)))
              vars))))
