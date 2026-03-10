(ns beichte.effect
  "Effect lattice and feature flags for static effect inference.

  ## State effect lattice (total order)

    :pure < :local < :mutation < :io

  - :pure      — no side effects, safe for AD, CSE, GPU, parallelization
  - :local     — thread-local mutation (transients, volatiles, loop scratch)
                  safe for parallelization/GPU, not for AD/CSE
  - :mutation  — global mutable state (atoms, refs, vars)
                  only safe for sequential execution
  - :io        — external I/O (println, file ops, network, system calls)
                  only safe for sequential, cannot reorder

  The join (least upper bound) of two effects is their maximum.
  Effect inference propagates the join through the call graph.

  ## Feature flags (orthogonal to state effects)

  Feature flags capture properties that are independent of the state
  effect ordering:

  - :throws    — may throw exceptions (partial function)
  - :random    — uses randomness (nondeterministic)
  - :reflects  — uses reflection (cannot AOT / cannot optimize)
  - :allocates — heap allocates (relevant for real-time / GPU scratch)

  Flags propagate via set union through the call graph.
  Compilation targets can declare which flags they tolerate."
  (:require [clojure.set]))

;; ================================================================
;; State effect lattice
;; ================================================================

(def ^:const levels
  "Effect levels in ascending order."
  [:pure :local :mutation :io])

(def ^:private level-ord
  {:pure 0 :local 1 :mutation 2 :io 3})

(defn level-index
  "Return the ordinal index of an effect level."
  [eff]
  (or (level-ord eff)
      (throw (ex-info (str "Unknown effect level: " eff)
                      {:effect eff :valid levels}))))

(defn join
  "Least upper bound of two effects (their maximum)."
  [a b]
  (if (>= (level-index a) (level-index b)) a b))

(defn join-all
  "Least upper bound of a collection of effects."
  ([effs] (reduce join :pure effs))
  ([init effs] (reduce join init effs)))

(defn <=effect
  "True if effect a is at most as effectful as b."
  [a b]
  (<= (level-index a) (level-index b)))

;; ================================================================
;; Feature flags
;; ================================================================

(def ^:const known-flags
  "Known feature flags."
  #{:throws :random :reflects :allocates})

(defn valid-flag?
  "True if flag is a recognized feature flag."
  [flag]
  (contains? known-flags flag))

(defn flags-join
  "Union of two flag sets."
  [a b]
  (into (or a #{}) b))

;; ================================================================
;; Combined effect descriptor
;; ================================================================

(defn effect
  "Create an effect descriptor: {:effect level, :flags #{...}}.
  Shorthand: (effect :pure) or (effect :local #{:throws})."
  ([level] {:effect level :flags #{}})
  ([level flags] {:effect level :flags (set flags)}))

(def pure
  "The pure effect descriptor (no effects, no flags)."
  (effect :pure))

(defn effect-join
  "Join two effect descriptors (component-wise max / union)."
  [a b]
  {:effect (join (:effect a :pure) (:effect b :pure))
   :flags  (flags-join (:flags a) (:flags b))})

(defn effect-join-all
  "Join a collection of effect descriptors."
  ([descs] (reduce effect-join pure descs))
  ([init descs] (reduce effect-join init descs)))

;; ================================================================
;; Budget checking
;; ================================================================

(defn within-budget?
  "True if the given effect level is within the budget.
  A budget is an effect level; anything at or below it is acceptable.

  Common budgets:
    :pure     — only pure code (AD, CSE)
    :local    — allow thread-local mutation (GPU, parallel)
    :mutation — allow global mutation (sequential, inlining)
    :io       — allow everything"
  [budget eff]
  (<=effect eff budget))

(defn budget-for
  "Return the maximum allowed effect level for a compilation target."
  [target]
  (case target
    :ad    :pure
    :cse   :pure
    :gpu   :local
    :simd  :local
    :parallel :local
    :sequential :io
    :inline :io
    (throw (ex-info (str "Unknown compilation target: " target)
                    {:target target
                     :valid [:ad :cse :gpu :simd :parallel :sequential :inline]}))))

(def ^:private default-unsupported-flags
  "Default unsupported flags per compilation target."
  {:ad       #{}          ;; AD can handle throws (just won't differentiate that path)
   :cse      #{:random}  ;; CSE assumes determinism — can't deduplicate nondeterministic calls
   :gpu      #{:throws :reflects}  ;; GPU kernels can't throw or reflect
   :simd     #{:reflects}
   :parallel #{}
   :sequential #{}
   :inline   #{}})

(defn unsupported-flags-for
  "Return the set of feature flags unsupported by a compilation target.
  Can be overridden by providing a custom map."
  ([target] (get default-unsupported-flags target #{}))
  ([target custom-map] (get custom-map target (unsupported-flags-for target))))

(defn compilable?
  "True if an effect descriptor is compatible with a compilation target.
  Checks both the state effect budget and feature flag support."
  ([target desc]
   (compilable? target desc nil))
  ([target desc custom-unsupported]
   (let [budget (budget-for target)
         unsupported (if custom-unsupported
                       (unsupported-flags-for target custom-unsupported)
                       (unsupported-flags-for target))]
     (and (within-budget? budget (:effect desc :pure))
          (empty? (clojure.set/intersection (:flags desc #{}) unsupported))))))
