(ns beichte.core
  "Static effect inference for Clojure — no type system required.

  Beichte (German: to confess) analyzes Clojure code via tools.analyzer.jvm
  and infers side-effect levels on a four-point lattice:

    :pure < :local < :mutation < :io

  Designed for compiler builders: validate that code is safe for AD,
  GPU compilation, parallelization, or CSE before transforming it.

  Quick start:

    (require '[beichte.core :as b])

    ;; Analyze expressions (fresh analysis each call — safe for REPL)
    (b/analyze '(+ 1 2))              ;; => :pure
    (b/analyze '(swap! a inc))        ;; => :mutation
    (b/analyze '(println \"hi\"))     ;; => :io
    (b/analyze '(aset arr 0 42))      ;; => :local

    ;; Full descriptors with feature flags
    (b/analyze-full '(throw (Exception. \"x\")))
    ;; => {:effect :pure, :flags #{:throws}}

    ;; Check if code is safe for a compilation target
    (b/compilable? :gpu '(+ 1 2))     ;; => true
    (b/compilable? :gpu '(throw (Exception. \"x\")))  ;; => false

    ;; Compiler pipelines: create a context to cache across calls
    (let [ctx (b/make-context)]
      (b/analyze '(foo x) ctx)      ;; analyzes foo from source
      (b/analyze '(bar (foo x)) ctx) ;; foo already cached
      (b/analyze-var #'baz ctx))     ;; works for vars too

  See beichte.effect for the lattice and flags, beichte.registry for
  the pre-annotated clojure.core entries, and beichte.analyze for the
  AST-walking inference engine."
  (:require [beichte.effect :as effect]
            [beichte.registry :as registry]
            [beichte.analyze :as ana]))

;; ================================================================
;; Context (explicit, opt-in caching)
;; ================================================================

(defn make-context
  "Create an analysis context that caches results across calls.

  For REPL use, omit the context — each call analyzes fresh.
  For compiler pipelines, create one context per pass and reuse it.

  Options:
    :registry — custom effect registry (default: default-registry)"
  ([] (ana/make-analysis))
  ([{:keys [registry]}]
   (if registry
     (ana/make-analysis registry)
     (ana/make-analysis))))

;; ================================================================
;; Primary API
;; ================================================================

(defn analyze
  "Infer the effect level of a Clojure expression.

  Returns one of: :pure, :local, :mutation, :io

  Without context: fresh analysis each call (safe for REPL).
  With context: reuses cached var analysis results."
  ([expr] (ana/analyze-expr expr {}))
  ([expr ctx-or-opts]
   (if (and (map? ctx-or-opts) (:cache ctx-or-opts))
     ;; It's a context from make-context
     (ana/analyze-expr expr {:analysis ctx-or-opts})
     ;; It's an opts map
     (ana/analyze-expr expr ctx-or-opts))))

(defn analyze-full
  "Infer the full effect descriptor of a Clojure expression.

  Returns {:effect :level, :flags #{...}} where flags are orthogonal
  properties like :throws, :random, :reflects, :allocates."
  ([expr] (ana/analyze-expr-full expr {}))
  ([expr ctx-or-opts]
   (if (and (map? ctx-or-opts) (:cache ctx-or-opts))
     (ana/analyze-expr-full expr {:analysis ctx-or-opts})
     (ana/analyze-expr-full expr ctx-or-opts))))

(defn analyze-var
  "Infer the effect level of a var by analyzing its source.

  Returns one of: :pure, :local, :mutation, :io"
  ([v] (ana/analyze-var v {}))
  ([v ctx-or-opts]
   (if (and (map? ctx-or-opts) (:cache ctx-or-opts))
     (ana/analyze-var v {:analysis ctx-or-opts})
     (ana/analyze-var v ctx-or-opts))))

(defn analyze-var-full
  "Infer the full effect descriptor of a var.

  Returns {:effect :level, :flags #{...}}."
  ([v] (ana/analyze-var-full v {}))
  ([v ctx-or-opts]
   (if (and (map? ctx-or-opts) (:cache ctx-or-opts))
     (ana/analyze-var-full v {:analysis ctx-or-opts})
     (ana/analyze-var-full v ctx-or-opts))))

(defn analyze-ns
  "Analyze all public vars in a namespace.

  Returns {var → effect-level}."
  ([ns-sym] (ana/analyze-ns ns-sym {}))
  ([ns-sym ctx-or-opts]
   (if (and (map? ctx-or-opts) (:cache ctx-or-opts))
     (ana/analyze-ns ns-sym {:analysis ctx-or-opts})
     (ana/analyze-ns ns-sym ctx-or-opts))))

(defn analyze-ns-full
  "Analyze all public vars and return {var → descriptor}."
  ([ns-sym] (ana/analyze-ns-full ns-sym {}))
  ([ns-sym ctx-or-opts]
   (if (and (map? ctx-or-opts) (:cache ctx-or-opts))
     (ana/analyze-ns-full ns-sym {:analysis ctx-or-opts})
     (ana/analyze-ns-full ns-sym ctx-or-opts))))

(defn compilable?
  "True if an expression is within the effect budget for a compilation target.

  Checks both state effect budget and feature flag support.

  Targets and their budgets:
    :ad, :cse       — :pure (no effects at all)
    :gpu, :simd     — :local (thread-local mutation OK)
    :parallel       — :local
    :sequential     — :io (anything goes)
    :inline         — :io (anything goes)

  GPU also rejects :throws and :reflects flags."
  ([target expr] (ana/compilable? target expr {}))
  ([target expr ctx-or-opts]
   (if (and (map? ctx-or-opts) (:cache ctx-or-opts))
     (ana/compilable? target expr {:analysis ctx-or-opts})
     (ana/compilable? target expr ctx-or-opts))))

(defn filter-compilable
  "Filter vars to those within the effect budget for a compilation target."
  ([target vars] (ana/filter-compilable target vars {}))
  ([target vars ctx-or-opts]
   (if (and (map? ctx-or-opts) (:cache ctx-or-opts))
     (ana/filter-compilable target vars {:analysis ctx-or-opts})
     (ana/filter-compilable target vars ctx-or-opts))))

;; ================================================================
;; Effect lattice (re-exported for convenience)
;; ================================================================

(def levels
  "Effect levels in ascending order: [:pure :local :mutation :io]"
  effect/levels)

(def flags
  "Known feature flags: #{:throws :random :reflects :allocates}"
  effect/known-flags)

(defn join
  "Least upper bound of two effects."
  [a b]
  (effect/join a b))

(defn within-budget?
  "True if effect level is at most as effectful as budget."
  [budget eff]
  (effect/within-budget? budget eff))

(defn budget-for
  "Return the effect budget for a compilation target."
  [target]
  (effect/budget-for target))

;; ================================================================
;; Registry management
;; ================================================================

(defn default-registry
  "Return the default registry with pre-annotated clojure.core and Java entries."
  []
  (registry/default-registry))

(defn make-registry
  "Create a custom registry from a map of {entry → effect-level}.
  Entries can be vars, [class method] pairs, or class name strings."
  [entries]
  (registry/make-registry entries))

(defn extend-registry
  "Extend a registry with additional entries."
  [reg entries]
  (registry/with-entries reg entries))
