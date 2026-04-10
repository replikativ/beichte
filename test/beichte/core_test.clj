(ns beichte.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [beichte.core :as b]
            [beichte.effect :as effect]
            [beichte.registry :as registry]
            [beichte.analyze :as ana]))

;; ================================================================
;; Effect lattice
;; ================================================================

(deftest effect-lattice-ordering
  (testing "level indices are ordered"
    (is (< (effect/level-index :pure) (effect/level-index :local)))
    (is (< (effect/level-index :local) (effect/level-index :mutation)))
    (is (< (effect/level-index :mutation) (effect/level-index :io))))

  (testing "join returns maximum"
    (is (= :pure (effect/join :pure :pure)))
    (is (= :local (effect/join :pure :local)))
    (is (= :local (effect/join :local :pure)))
    (is (= :mutation (effect/join :local :mutation)))
    (is (= :io (effect/join :mutation :io)))
    (is (= :io (effect/join :pure :io))))

  (testing "join-all"
    (is (= :pure (effect/join-all [])))
    (is (= :pure (effect/join-all [:pure :pure :pure])))
    (is (= :io (effect/join-all [:pure :local :mutation :io])))
    (is (= :local (effect/join-all [:pure :local :pure]))))

  (testing "<=effect"
    (is (effect/<=effect :pure :pure))
    (is (effect/<=effect :pure :io))
    (is (not (effect/<=effect :io :pure)))
    (is (effect/<=effect :local :mutation)))

  (testing "within-budget?"
    (is (effect/within-budget? :pure :pure))
    (is (not (effect/within-budget? :pure :local)))
    (is (effect/within-budget? :local :local))
    (is (effect/within-budget? :local :pure))
    (is (not (effect/within-budget? :local :mutation)))
    (is (effect/within-budget? :io :io))))

(deftest effect-budgets
  (testing "compilation target budgets"
    (is (= :pure (effect/budget-for :ad)))
    (is (= :pure (effect/budget-for :cse)))
    (is (= :local (effect/budget-for :gpu)))
    (is (= :local (effect/budget-for :simd)))
    (is (= :local (effect/budget-for :parallel)))
    (is (= :io (effect/budget-for :sequential)))
    (is (= :io (effect/budget-for :inline)))))

;; ================================================================
;; Registry
;; ================================================================

(deftest registry-basics
  (testing "default registry has clojure.core annotations"
    (let [reg (registry/default-registry)]
      (is (= :pure (registry/lookup reg #'clojure.core/+)))
      (is (= :pure (registry/lookup reg #'clojure.core/map)))
      (is (= :local (registry/lookup reg #'clojure.core/aset)))
      (is (= :mutation (registry/lookup reg #'clojure.core/swap!)))
      (is (= :io (registry/lookup reg #'clojure.core/println)))))

  (testing "extend registry"
    (let [reg (registry/default-registry)
          ext (registry/with-entries reg {:my-custom-entry :pure})]
      (is (= :pure (registry/lookup ext :my-custom-entry)))
      ;; Original entries preserved
      (is (= :pure (registry/lookup ext #'clojure.core/+)))))

  (testing "java method annotations"
    (let [reg (registry/default-registry)]
      (is (= :pure (registry/lookup reg ["java.lang.Math" 'sin])))
      (is (= :pure (registry/lookup reg ["clojure.lang.Numbers" 'add])))
      (is (= :pure (registry/lookup reg ["clojure.lang.RT" 'seq]))))))

;; ================================================================
;; Expression analysis
;; ================================================================

(deftest pure-expressions
  (testing "arithmetic is pure"
    (is (= :pure (b/analyze '(+ 1 2))))
    (is (= :pure (b/analyze '(* 3.0 (+ 1.0 2.0)))))
    (is (= :pure (b/analyze '(inc (dec 5))))))

  (testing "math functions are pure"
    (is (= :pure (b/analyze '(Math/sin 1.0))))
    (is (= :pure (b/analyze '(Math/sqrt (Math/pow 3.0 2.0))))))

  (testing "collection operations are pure"
    (is (= :pure (b/analyze '(assoc {:a 1} :b 2))))
    (is (= :pure (b/analyze '(conj [1 2] 3))))
    (is (= :pure (b/analyze '(get {:a 1} :a)))))

  (testing "constants are pure"
    (is (= :pure (b/analyze '42)))
    (is (= :pure (b/analyze '"hello")))
    (is (= :pure (b/analyze ':keyword)))
    (is (= :pure (b/analyze 'nil))))

  (testing "let bindings are pure when body is pure"
    (is (= :pure (b/analyze '(let [x 1 y 2] (+ x y))))))

  (testing "if is pure when all branches are pure"
    (is (= :pure (b/analyze '(if true 1 2)))))

  (testing "fn is pure when body is pure"
    (is (= :pure (b/analyze '(fn [x] (+ x 1)))))))

(deftest impure-expressions
  (testing "println is :io"
    (is (= :io (b/analyze '(println "hello")))))

  (testing "swap! is :mutation"
    (is (= :mutation (b/analyze '(swap! (atom 0) inc)))))

  (testing "reset! is :mutation"
    (is (= :mutation (b/analyze '(reset! (atom 0) 42)))))

  (testing "impurity propagates through let"
    (is (= :io (b/analyze '(let [x (println "side effect")] x)))))

  (testing "impurity propagates through if"
    (is (= :io (b/analyze '(if true (println "yes") 0))))))

(deftest local-mutation-expressions
  (testing "aset is :local"
    (is (= :local (b/analyze '(aset (int-array 10) 0 42)))))

  (testing "transient operations are :local"
    (is (= :local (b/analyze '(persistent! (conj! (transient []) 1)))))))

;; ================================================================
;; Var analysis
;; ================================================================

(defn- pure-helper [x] (+ x 1))
(defn- impure-helper [x] (println x) x)

(deftest var-analysis
  (testing "known pure vars"
    (is (= :pure (b/analyze-var #'clojure.core/+)))
    (is (= :pure (b/analyze-var #'clojure.core/map))))

  (testing "known impure vars"
    (is (= :io (b/analyze-var #'clojure.core/println)))
    (is (= :mutation (b/analyze-var #'clojure.core/swap!))))

  (testing "user-defined pure function"
    (is (= :pure (b/analyze-var #'pure-helper))))

  (testing "user-defined impure function"
    (is (= :io (b/analyze-var #'impure-helper)))))

;; ================================================================
;; Compilability checks
;; ================================================================

(deftest compilability
  (testing "pure code is compilable for all targets"
    (is (b/compilable? :ad '(+ 1 2)))
    (is (b/compilable? :gpu '(+ 1 2)))
    (is (b/compilable? :sequential '(+ 1 2))))

  (testing "local mutation is OK for GPU, not for AD"
    (is (b/compilable? :gpu '(aset (int-array 10) 0 42)))
    (is (not (b/compilable? :ad '(aset (int-array 10) 0 42)))))

  (testing "global mutation is not OK for GPU"
    (is (not (b/compilable? :gpu '(swap! (atom 0) inc))))
    (is (b/compilable? :sequential '(swap! (atom 0) inc))))

  (testing "I/O is only OK for sequential/inline"
    (is (not (b/compilable? :gpu '(println "hi"))))
    (is (not (b/compilable? :parallel '(println "hi"))))
    (is (b/compilable? :sequential '(println "hi")))))

;; ================================================================
;; Feature flags
;; ================================================================

(deftest feature-flags
  (testing "throw adds :throws flag"
    (let [desc (b/analyze-full '(throw (Exception. "boom")))]
      (is (= :pure (:effect desc)))
      (is (contains? (:flags desc) :throws))))

  (testing "pure code has no flags"
    (let [desc (b/analyze-full '(+ 1 2))]
      (is (= :pure (:effect desc)))
      (is (empty? (:flags desc)))))

  (testing "throw in impure context preserves both"
    (let [desc (b/analyze-full '(do (println "x") (throw (Exception. "y"))))]
      (is (= :io (:effect desc)))
      (is (contains? (:flags desc) :throws))))

  (testing "GPU rejects :throws"
    (is (not (b/compilable? :gpu '(throw (Exception. "x"))))))

  (testing "AD accepts :throws (divergence is OK for AD)"
    (is (b/compilable? :ad '(throw (Exception. "x")))))

  (testing "rand adds :random flag"
    (let [desc (b/analyze-full '(rand))]
      (is (contains? (:flags desc) :random))))

  (testing "rand-int adds :random flag"
    (let [desc (b/analyze-full '(rand-int 10))]
      (is (contains? (:flags desc) :random))))

  (testing "sequential accepts everything"
    (is (b/compilable? :sequential '(do (println "x") (throw (Exception. "y")))))))

(deftest effect-descriptors
  (testing "effect-join combines levels and flags"
    (let [a (effect/effect :pure #{:throws})
          b (effect/effect :local #{:random})
          joined (effect/effect-join a b)]
      (is (= :local (:effect joined)))
      (is (= #{:throws :random} (:flags joined)))
      (is (= #{} (:writes joined)))))

  (testing "effect-join-all"
    (is (= effect/pure (effect/effect-join-all [])))
    (let [joined (effect/effect-join-all
                  [(effect/effect :pure #{:throws})
                   (effect/effect :io)])]
      (is (= :io (:effect joined)))
      (is (= #{:throws} (:flags joined))))))

;; ================================================================
;; Scope-aware write tracking
;; ================================================================

(deftest scope-aware-scratch-arrays
  (testing "scratch array: write to fresh local that doesn't escape → :pure"
    (is (= :pure (:effect (b/analyze-full
                           '(fn [x] (let [tmp (double-array 1)]
                                      (clojure.core/aset tmp 0 (* x x))
                                      (clojure.core/aget tmp 0))))))))

  (testing "nested scratch: inner let scope resolves independently"
    (is (= :pure (:effect (b/analyze-full
                           '(fn [x] (let [result (let [tmp (double-array 1)]
                                                   (clojure.core/aset tmp 0 x)
                                                   (clojure.core/aget tmp 0))]
                                      result)))))))

  (testing "multiple scratch arrays: all invisible"
    (is (= :pure (:effect (b/analyze-full
                           '(fn [x y] (let [a (double-array 1)
                                            b (double-array 1)]
                                        (clojure.core/aset a 0 x)
                                        (clojure.core/aset b 0 y)
                                        (+ (clojure.core/aget a 0)
                                           (clojure.core/aget b 0))))))))))

(deftest scope-aware-construction
  (testing "fresh array returned: write to fresh that escapes → :local"
    (is (= :local (:effect (b/analyze-full
                            '(fn [n] (let [out (double-array n)]
                                       (clojure.core/aset out 0 42.0)
                                       out)))))))

  (testing "loop-filled array returned: construction pattern → :local"
    (is (= :local (:effect (b/analyze-full
                            '(fn [n] (let [out (double-array n)]
                                       (loop [i 0]
                                         (when (< i n)
                                           (clojure.core/aset out i (double i))
                                           (recur (inc i))))
                                       out)))))))

  (testing "conditional return of fresh array → :local (may escape)"
    (is (= :local (:effect (b/analyze-full
                            '(fn [n] (let [out (double-array n)]
                                       (clojure.core/aset out 0 1.0)
                                       (if (> n 0) out nil)))))))))

(deftest scope-aware-param-mutation
  (testing "write to parameter → :mutation"
    (is (= :mutation (:effect (b/analyze-full
                               '(fn [param] (clojure.core/aset param 0 42.0) param))))))

  (testing "alias to param → :mutation (follows alias chain)"
    (is (= :mutation (:effect (b/analyze-full
                               '(fn [arr] (let [alias arr]
                                            (clojure.core/aset alias 0 42.0)
                                            alias)))))))

  (testing "swap! on param → :mutation"
    (is (= :mutation (:effect (b/analyze-full
                               '(fn [a] (swap! a inc))))))))

(deftest scope-aware-transients
  (testing "transient construction: conj! into fresh transient → :local"
    ;; transient creates fresh, conj! writes to it, persistent! returns
    (is (= :local (:effect (b/analyze-full
                            '(fn [xs] (persistent! (conj! (transient []) 1)))))))))

(deftest scope-aware-no-writes
  (testing "pure arithmetic unchanged"
    (is (= :pure (:effect (b/analyze-full '(fn [x y] (+ x y)))))))

  (testing "println still :io"
    (is (= :io (:effect (b/analyze-full '(fn [] (println "hello")))))))

  (testing "map/filter still :pure"
    (is (= :pure (:effect (b/analyze-full '(fn [xs] (map inc xs))))))))

;; ================================================================
;; Context (explicit caching)
;; ================================================================

(deftest context-caching
  (testing "context reuses cached results"
    (let [ctx (b/make-context)]
      ;; First call analyzes pure-helper from source
      (is (= :pure (b/analyze-var #'pure-helper ctx)))
      ;; Second call hits cache (we verify by checking cache atom)
      (is (= :pure (b/analyze-var #'pure-helper ctx)))
      ;; Cache should contain the var
      (is (contains? @(:cache ctx) #'pure-helper))))

  (testing "without context, no cross-call caching"
    ;; Each call creates fresh analysis — no shared state
    (is (= :pure (b/analyze '(+ 1 2))))
    (is (= :io (b/analyze '(println "hi")))))

  (testing "context with custom registry"
    (let [reg (b/extend-registry (b/default-registry)
                                 {:my-entry :pure})
          ctx (b/make-context {:registry reg})]
      ;; Custom entry is visible through context
      (is (= :pure (get (:registry ctx) :my-entry)))))

  (testing "context caches var analysis across calls"
    (let [ctx (b/make-context)]
      (b/analyze-var #'pure-helper ctx)
      (b/analyze-var #'impure-helper ctx)
      ;; Both should be cached now
      (is (contains? @(:cache ctx) #'pure-helper))
      (is (contains? @(:cache ctx) #'impure-helper))
      ;; Cached values are correct
      (is (= :pure (b/analyze-var #'pure-helper ctx)))
      (is (= :io (b/analyze-var #'impure-helper ctx))))))

;; ================================================================
;; Public API surface
;; ================================================================

(deftest public-api
  (testing "levels constant"
    (is (= [:pure :local :mutation :io] b/levels)))

  (testing "flags constant"
    (is (contains? b/flags :throws))
    (is (contains? b/flags :random))
    (is (contains? b/flags :reflects))
    (is (contains? b/flags :allocates)))

  (testing "join convenience"
    (is (= :mutation (b/join :local :mutation))))

  (testing "within-budget? convenience"
    (is (b/within-budget? :local :pure))
    (is (not (b/within-budget? :pure :local))))

  (testing "budget-for convenience"
    (is (= :pure (b/budget-for :ad)))))
