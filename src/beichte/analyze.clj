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
  - `infer-descriptor` — returns full descriptor with scope-aware write tracking

  The descriptor mode tracks which binding names are written to during
  analysis.  At scope boundaries (let, fn-method), writes are resolved:
  writes to locally-allocated bindings that don't escape are forgotten
  (the mutation is invisible), while writes to function parameters or
  aliases thereof promote the effect to :mutation."
  (:require [beichte.effect :as effect]
            [beichte.registry :as registry]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.jvm :as jvm]
            [clojure.java.io :as io]
            [clojure.set :as set])
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
;; Scope tracking for write-set analysis
;; ================================================================

(def empty-scope
  "An empty scope with no bindings and no parameters."
  {:provenance {}   ;; {sym → :fresh | :external | sym (alias target)}
   :params     #{}  ;; function parameter names (always :external)
   })

(defn- resolve-provenance
  "Resolve a symbol's provenance through alias chains.
  Returns :fresh or :external."
  [scope sym]
  (loop [s sym seen #{}]
    (if (contains? seen s)
      :external  ;; cycle → conservative
      (let [p (get (:provenance scope) s)]
        (cond
          (= :fresh p) :fresh
          (= :external p) :external
          (symbol? p) (recur p (conj seen s))  ;; follow alias chain
          ;; Not in provenance map — check if it's a param
          (nil? p) (if (contains? (:params scope) s) :external :external)
          :else :external)))))

(def ^:private allocation-methods
  "Static methods known to produce fresh allocations."
  #{["clojure.lang.Numbers" 'double_array]
    ["clojure.lang.Numbers" 'float_array]
    ["clojure.lang.Numbers" 'int_array]
    ["clojure.lang.Numbers" 'long_array]
    ["clojure.lang.Numbers" 'byte_array]
    ["clojure.lang.Numbers" 'short_array]
    ["clojure.lang.Numbers" 'char_array]
    ["clojure.lang.Numbers" 'boolean_array]
    ["clojure.lang.RT" 'aclone]})

(def ^:private allocation-vars
  "Vars known to produce fresh allocations."
  #{#'clojure.core/make-array #'clojure.core/object-array
    #'clojure.core/int-array #'clojure.core/long-array
    #'clojure.core/float-array #'clojure.core/double-array
    #'clojure.core/byte-array #'clojure.core/short-array
    #'clojure.core/char-array #'clojure.core/boolean-array
    #'clojure.core/aclone
    #'clojure.core/transient #'clojure.core/volatile!
    #'clojure.core/atom
    #'clojure.core/vec #'clojure.core/vector
    #'clojure.core/hash-map #'clojure.core/array-map
    #'clojure.core/hash-set #'clojure.core/sorted-set})

(defn- fresh-allocation?
  "True if this AST node produces a fresh, unaliased reference."
  [ast]
  (case (:op ast)
    :new true
    :static-call (contains? allocation-methods
                            [(pr-str (:class ast)) (:method ast)])
    :invoke (when-let [v (-> ast :fn :var)]
              (contains? allocation-vars v))
    ;; vector/map/set literals produce fresh values
    (:vector :map :set) true
    false))

(defn- classify-binding-provenance
  "Determine provenance of a binding's init expression.
  Returns :fresh, :external, or a symbol (alias target)."
  [init-ast]
  (cond
    ;; Fresh allocation
    (fresh-allocation? init-ast)
    :fresh

    ;; Direct reference to another binding — alias
    (= :local (:op init-ast))
    (:name init-ast)  ;; alias → follow this chain later

    ;; Everything else (function calls, conditionals, etc.) — conservative
    :else :external))

(def ^:private write-target-methods
  "Static methods that write to their first argument."
  #{["clojure.lang.RT" 'aset]})

(def ^:private write-target-vars
  "Vars whose invocation writes to their first argument."
  #{#'clojure.core/aset
    #'clojure.core/aset-int #'clojure.core/aset-long
    #'clojure.core/aset-float #'clojure.core/aset-double
    #'clojure.core/aset-byte #'clojure.core/aset-short
    #'clojure.core/aset-char #'clojure.core/aset-boolean
    #'clojure.core/conj! #'clojure.core/assoc! #'clojure.core/dissoc!
    #'clojure.core/pop! #'clojure.core/disj!
    #'clojure.core/vreset! #'clojure.core/vswap!
    #'clojure.core/swap! #'clojure.core/reset!})

(defn- extract-write-target
  "If this AST node is a write operation, return the name of the
  binding being written to, or nil."
  [ast]
  (case (:op ast)
    :static-call
    (when (and (contains? write-target-methods
                          [(pr-str (:class ast)) (:method ast)])
               (seq (:args ast))
               (= :local (:op (first (:args ast)))))
      (:name (first (:args ast))))

    :invoke
    (when-let [v (-> ast :fn :var)]
      (when (and (contains? write-target-vars v)
                 (seq (:args ast))
                 (= :local (:op (first (:args ast)))))
        (:name (first (:args ast)))))

    :set!
    (when (= :local (:op (:target ast)))
      (:name (:target ast)))

    nil))

(defn- escaping-locals
  "Collect locals whose references could escape as the return value of this
  expression.  Only tracks value-position flow — a local used as an argument
  to a function call (like aget) does NOT escape because the call returns its
  own result, not the argument reference.

  This is used to decide whether a fresh binding's reference leaves the scope
  (construction pattern, :local) or stays internal (scratch, :pure)."
  [ast]
  (case (:op ast)
    nil          #{}
    :local       #{(:name ast)}
    ;; do: only the ret part is in value position
    :do          (escaping-locals (:ret ast))
    ;; if: both branches are in value position
    :if          (into (escaping-locals (:then ast))
                       (escaping-locals (:else ast)))
    ;; let/loop: the body is in value position
    (:let :loop) (escaping-locals (:body ast))
    ;; case: all branches + default
    :case        (reduce into
                         (escaping-locals (:default ast))
                         (map escaping-locals (:thens ast)))
    ;; letfn: body is value position
    :letfn       (escaping-locals (:body ast))
    ;; with-meta: the expr is value position
    :with-meta   (escaping-locals (:expr ast))
    ;; try: body + catches + finally all contribute
    :try         (reduce into
                         (escaping-locals (:body ast))
                         (concat (map escaping-locals (:catches ast))
                                 (when (:finally ast)
                                   [(escaping-locals (:finally ast))])))
    :catch       (escaping-locals (:body ast))
    ;; Function calls, static-call, new, etc. — they return their own value,
    ;; arguments do not escape through the return value
    #{}))

(defn- resolve-scope-writes
  "At a let scope boundary, resolve writes against provenance:
  - Writes to :fresh syms that don't escape → forget (invisible, :pure)
  - Writes to :fresh syms that escape (return value) → keep as :local (construction)
  - Writes to :external syms → promote to :mutation

  Also downgrades :local effects that came from fresh allocations when the
  allocation stays internal (its mutation is invisible to the caller)."
  [desc scope let-bound-syms escaping-syms]
  (let [writes (:writes desc #{})
        ;; Partition writes by whether they target let-bound symbols
        {let-writes true outer-writes false}
        (group-by #(contains? let-bound-syms %) writes)
        ;; Resolve let-bound writes
        resolved-let-writes
        (reduce (fn [acc w]
                  (let [prov (resolve-provenance scope w)]
                    (cond
                      ;; Fresh + doesn't escape → invisible, forget
                      (and (= :fresh prov)
                           (not (contains? escaping-syms w)))
                      (update acc :invisible conj w)
                      ;; Fresh + escapes → construction, keep as :local
                      (= :fresh prov)
                      (update acc :local-writes conj w)
                      ;; External → mutation
                      :else
                      (update acc :mutation-writes conj w))))
                {:invisible #{} :local-writes #{} :mutation-writes #{}}
                (or let-writes []))
        ;; Only outer writes propagate — let-bound writes are fully resolved here.
        ;; Fresh+escape writes have been accounted for as :local effect,
        ;; and fresh+invisible writes are forgotten.
        remaining-writes (set (or outer-writes []))
        has-mutation? (seq (:mutation-writes resolved-let-writes))
        has-construction? (seq (:local-writes resolved-let-writes))
        ;; When all writes to fresh locals are invisible and their allocation
        ;; effects are the only :local contributor, downgrade to :pure
        all-fresh-invisible? (and (seq (:invisible resolved-let-writes))
                                  (not has-construction?)
                                  (not has-mutation?)
                                  (empty? (or outer-writes [])))
        new-effect (cond
                     has-mutation? (effect/join (:effect desc) :mutation)
                     ;; Construction: fresh alloc + write + return → :local
                     has-construction? (effect/join (:effect desc) :local)
                     ;; If all local writes were to invisible fresh bindings,
                     ;; the :local effect from their allocation is also invisible
                     all-fresh-invisible? (let [base (:effect desc)]
                                            (if (= :local base) :pure base))
                     :else (:effect desc))]
    (-> desc
        (assoc :writes remaining-writes)
        (assoc :effect new-effect))))

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
  [analysis visited scope nodes]
  (reduce (fn [acc node]
            (if node
              (effect/effect-join acc (infer-descriptor analysis visited scope node))
              acc))
          effect/pure
          nodes))

(defn- infer-var-descriptor
  "Infer the full descriptor of a var (with flags)."
  [analysis visited scope ast]
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
              (let [desc (infer-descriptor analysis (conj visited v) empty-scope analyzed)
                    desc (update desc :flags into base-flags)
                    ;; Strip writes before caching — var-level results are resolved
                    desc (effect/strip-writes desc)]
                (cache-effect! analysis v desc)
                desc)
              (let [desc (effect/effect :io base-flags)]
                (cache-effect! analysis v desc)
                desc))))))))

(defn infer-descriptor
  "Infer the full effect descriptor of an AST node.
  Returns {:effect :level, :flags #{...}, :writes #{...}}.
  The :writes set tracks binding names written to in the current scope;
  it is resolved at let and fn-method boundaries."
  ([analysis ast]
   (infer-descriptor analysis #{} empty-scope ast))
  ([analysis visited scope ast]
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
               ;; Track write target for mutation ops (e.g. RT/aset)
               write-sym (extract-write-target ast)
               method-desc (cond
                             ;; When write target is tracked, cap effect at :local —
                             ;; scope resolution will determine the actual level
                             write-sym  (effect/effect :local
                                                       (cond-> (:flags (or known-desc {}))
                                                         rand? (conj :random)))
                             known-desc (if rand?
                                          (update known-desc :flags conj :random)
                                          known-desc)
                             rand?      (effect/effect :pure #{:random})
                             :else      (effect/effect :io))
               base-desc (effect/effect-join method-desc
                                             (infer-descriptors-join analysis visited scope args))]
           (if write-sym
             (update base-desc :writes (fnil conj #{}) write-sym)
             base-desc))

         :const
         (case (:type ast)
           :class (let [class-str (pr-str (:form ast))
                        known (cached-effect analysis class-str)]
                    (if (nil? known) (effect/effect :io) (effect/effect (or known :pure))))
           (:vector :map :set :nil :number :keyword :string :bool :constant :var :type :record :seq :fn :char :regex :unknown) effect/pure)

         :def
         (infer-descriptor analysis (conj visited (:var ast)) scope
                           (or (-> ast :init :expr) (-> ast :init)))

         :fn (infer-descriptors-join analysis visited scope (:methods ast))

         :fn-method
         ;; Function boundary: create a new scope with params as :external.
         ;; After analyzing the body, resolve writes: writes to params → :mutation.
         (let [param-names (set (map :name (:params ast)))
               fn-scope {:provenance (zipmap param-names (repeat :external))
                         :params param-names}
               body-desc (infer-descriptor analysis visited fn-scope (:body ast))
               ;; Resolve: any write to a param (or alias to param) → :mutation
               writes (:writes body-desc #{})
               has-param-writes? (some (fn [w]
                                         (= :external (resolve-provenance fn-scope w)))
                                       writes)
               resolved-effect (if has-param-writes?
                                 (effect/join (:effect body-desc) :mutation)
                                 (:effect body-desc))]
           (-> body-desc
               (assoc :effect resolved-effect)
               (assoc :writes #{})))  ;; writes don't propagate past fn boundary

         :do (effect/effect-join
              (infer-descriptors-join analysis visited scope (:statements ast))
              (infer-descriptor analysis visited scope (:ret ast)))

         :local (if (:assignable? ast)
                  (effect/effect :local)
                  effect/pure)

         :invoke
         (let [;; Track write target for mutation vars (e.g. conj!, swap!)
               write-sym (extract-write-target ast)
               fn-desc (if write-sym
                         ;; When write target is tracked, cap function's effect at :local
                         ;; — scope resolution will determine the actual level
                         (effect/effect :local)
                         (infer-descriptor analysis visited scope (:fn ast)))
               base-desc (effect/effect-join fn-desc
                                             (infer-descriptors-join analysis visited scope args))]
           (if write-sym
             (update base-desc :writes (fnil conj #{}) write-sym)
             base-desc))

         :try (effect/effect-join-all
               [(infer-descriptor analysis visited scope (:body ast))
                (infer-descriptors-join analysis visited scope (:catches ast))
                (infer-descriptor analysis visited scope (:finally ast))])

         :catch (infer-descriptor analysis visited scope (:body ast))
         :instance? effect/pure

         :let
         ;; Scope boundary: process bindings sequentially (each extends scope),
         ;; then resolve writes at exit.
         (let [[bindings-desc scope']
               (reduce
                (fn [[desc scope] binding-ast]
                  (let [init-ast (:init binding-ast)
                        init-desc (infer-descriptor analysis visited scope init-ast)
                        sym (:name binding-ast)
                        prov (classify-binding-provenance init-ast)
                        scope' (assoc-in scope [:provenance sym] prov)]
                    [(effect/effect-join desc init-desc) scope']))
                [effect/pure scope]
                (:bindings ast))
               body-desc (infer-descriptor analysis visited scope' (:body ast))
               combined (effect/effect-join bindings-desc body-desc)
               ;; Resolve writes at scope exit
               let-bound-syms (set (map :name (:bindings ast)))
               escaping (escaping-locals (:body ast))]
           (resolve-scope-writes combined scope' let-bound-syms escaping))

         :binding (infer-descriptor analysis visited scope (:init ast))

         :instance-call
         (let [class-str (pr-str (:class ast))
               method (:method ast)
               key [class-str method]
               known-desc (cached-descriptor analysis key)
               method-desc (or known-desc (effect/effect :io))]
           (effect/effect-join method-desc
                               (infer-descriptors-join analysis visited scope args)))

         :loop
         ;; Loop bindings create a scope similar to let
         (let [[bindings-desc scope']
               (reduce
                (fn [[desc scope] binding-ast]
                  (let [init-ast (:init binding-ast)
                        init-desc (infer-descriptor analysis visited scope init-ast)
                        sym (:name binding-ast)
                        prov (classify-binding-provenance init-ast)
                        scope' (assoc-in scope [:provenance sym] prov)]
                    [(effect/effect-join desc init-desc) scope']))
                [effect/pure scope]
                (:bindings ast))
               body-desc (infer-descriptor analysis visited scope' (:body ast))
               combined (effect/effect-join bindings-desc body-desc)
               let-bound-syms (set (map :name (:bindings ast)))
               escaping (escaping-locals (:body ast))]
           (resolve-scope-writes combined scope' let-bound-syms escaping))

         :recur (infer-descriptors-join analysis visited scope (:exprs ast))

         :new (effect/effect-join
               (infer-descriptor analysis visited scope (:class ast))
               (infer-descriptors-join analysis visited scope args))

         :if (effect/effect-join-all
              [(infer-descriptor analysis visited scope (:test ast))
               (infer-descriptor analysis visited scope (:then ast))
               (infer-descriptor analysis visited scope (:else ast))])

         :vector (infer-descriptors-join analysis visited scope (:items ast))
         :set (infer-descriptors-join analysis visited scope (:items ast))
         :map (effect/effect-join
               (infer-descriptors-join analysis visited scope (:keys ast))
               (infer-descriptors-join analysis visited scope (:vals ast)))

         (:var :the-var)
         (if (contains? visited (:var ast))
           effect/pure
           (infer-var-descriptor analysis visited scope ast))

         :static-field effect/pure
         :instance-field (effect/effect :mutation)

         :with-meta (effect/effect-join
                     (infer-descriptor analysis visited scope (:expr ast))
                     (infer-descriptor analysis visited scope (:meta ast)))

         :host-interop (infer-descriptors-join analysis visited scope args)
         :keyword-invoke (infer-descriptors-join analysis visited scope args)
         :protocol-invoke (infer-descriptors-join analysis visited scope args)
         :prim-invoke (infer-descriptors-join analysis visited scope args)

         :monitor-enter (effect/effect :mutation)
         :monitor-exit (effect/effect :mutation)

         ;; throw — state effect comes from the exception expr,
         ;; but adds :throws flag
         :throw (update (infer-descriptor analysis visited scope (:exception ast))
                        :flags conj :throws)

         :set!
         (let [write-sym (when (= :local (:op (:target ast)))
                           (:name (:target ast)))
               desc (effect/effect :mutation)]
           (if write-sym
             (update desc :writes conj write-sym)
             desc))

         :case (effect/effect-join-all
                [(infer-descriptor analysis visited scope (:test ast))
                 (infer-descriptors-join analysis visited scope (:thens ast))
                 (infer-descriptor analysis visited scope (:default ast))])

         :letfn (effect/effect-join
                 (infer-descriptors-join analysis visited scope (:bindings ast))
                 (infer-descriptor analysis visited scope (:body ast)))

         :method (infer-descriptor analysis visited scope (:body ast))

         ;; Unknown op
         (effect/effect :io))))))

;; ================================================================
;; High-level API
;; ================================================================

(defn- make-ast
  "Build an AST for an expression, optionally with local bindings.
  locals: a set or seq of symbols to treat as locally-bound (won't fail on resolve)."
  [expr ns-str locals]
  (let [local-map (if (map? locals)
                    locals
                    (into {} (map (fn [s]
                                    [(symbol (name s))
                                     {:op :binding :name (symbol (name s))
                                      :form (symbol (name s)) :local :let}])
                                  locals)))
        env (cond-> (assoc (jvm/empty-env) :ns (symbol ns-str))
              (seq local-map) (assoc :locals local-map))
        wrapped `(fn [] ~expr)]
    (jvm/analyze wrapped env)))

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
    :ns       — namespace context (default: *ns*)
    :locals   — set or seq of symbols to treat as locally-bound"
  ([expr] (analyze-expr expr {}))
  ([expr opts]
   (let [ns-str (or (:ns opts) (str (clojure.core/ns-name *ns*)))
         analysis (resolve-analysis opts)
         ast (make-ast expr ns-str (:locals opts))]
     (infer-effect analysis ast))))

(defn analyze-expr-full
  "Analyze an S-expression and return its full effect descriptor.
  Returns {:effect :level, :flags #{...}}.

  opts may include:
    :analysis — reuse a context from make-analysis (for caching)
    :registry — custom registry (ignored if :analysis given)
    :ns       — namespace context (default: *ns*)
    :locals   — set or seq of symbols to treat as locally-bound"
  ([expr] (analyze-expr-full expr {}))
  ([expr opts]
   (let [ns-str (or (:ns opts) (str (clojure.core/ns-name *ns*)))
         analysis (resolve-analysis opts)
         ast (make-ast expr ns-str (:locals opts))]
     (effect/strip-writes (infer-descriptor analysis ast)))))

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
         (infer-var-descriptor analysis #{} empty-scope {:var v})))))

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
                         (infer-var-descriptor analysis #{} empty-scope {:var v}))]))
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
                               (infer-var-descriptor analysis #{} empty-scope {:var v}))]
                  (effect/compilable? target desc)))
              vars))))
