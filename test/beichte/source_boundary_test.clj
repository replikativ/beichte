(ns beichte.source-boundary-test
  (:require [clojure.test :refer [deftest is]]
            [beichte.analyze :as analyze]
            [beichte.core :as b]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as jvm]))

(defn unknown-callee [value] (inc value))
(defn primitive-callee ^long [^long value] value)
(defprotocol BoundaryProtocol
  (boundary-call [receiver value]))

(def reader-events (atom 0))
(defn reader-hit! []
  (swap! reader-events inc)
  '(defn unknown-callee [value] value))

(defn- level [result]
  (if (map? result) (:effect result) result))

(def ^:private apis
  [[b/analyze b/analyze-var] [b/analyze-full b/analyze-var-full]])

(defn- context
  ([] (b/make-context {:source-resolver nil}))
  ([entries]
   (b/make-context {:source-resolver nil
                    :registry (b/extend-registry (b/default-registry) entries)})))

(deftest source-resolution-is-an-explicit-context-boundary
  (doseq [[expression-api var-api] apis]
    (let [disabled (context)]
      (is (contains? disabled :source-resolver))
      (is (nil? (:source-resolver disabled)))
      (is (= :io (level (var-api #'unknown-callee disabled))))
      (is (= :io (level (expression-api '(beichte.source-boundary-test/unknown-callee 1)
                                        (context))))))
    (is (= :io (level (var-api #'unknown-callee {:source-resolver nil}))))
    (is (= :pure (level (var-api #'unknown-callee (b/make-context))))
        "ordinary tooling retains its default source lookup")
    (is (= :pure (level (expression-api '(beichte.source-boundary-test/unknown-callee 1)
                                        (context {#'unknown-callee :pure})))))
    (is (= :pure (level (expression-api '(clojure.core/+ 1 2) (context)))))
    (let [requested (atom [])
          resolver (fn [v]
                     (swap! requested conj v)
                     "(defn unknown-callee [value] (inc value))")
          analysis (b/make-context {:source-resolver resolver})]
      (is (identical? resolver (:source-resolver analysis)))
      (is (= :pure (level (var-api #'unknown-callee analysis))))
      (is (= [#'unknown-callee] @requested)))))

(deftest declarations-are-rejected-before-analyzer-macroexpansion
  (doseq [[_ var-api] apis
          source ["(defprotocol InjectedProtocol (injected [receiver]))"
                  "(deftype InjectedType [value])"
                  "(defrecord InjectedRecord [value])"
                  "(definterface InjectedInterface (injected []))"]]
    (let [analyzed (atom 0)
          analysis (b/make-context {:source-resolver (constantly source)})]
      (with-redefs [jvm/analyze (fn [& _] (swap! analyzed inc)
                                  (throw (AssertionError. "declaration reached analyzer")))]
        (is (= :io (level (var-api #'unknown-callee analysis)))))
      (is (zero? @analyzed)))))

(deftest resolver-source-never-enables-reader-evaluation
  (doseq [[_ var-api] apis]
    (let [before @reader-events
          analysis (b/make-context
                    {:source-resolver
                     (constantly "#=(beichte.source-boundary-test/reader-hit!)")})]
      (binding [*read-eval* true]
        (is (= :io (level (var-api #'unknown-callee analysis)))))
      (is (= before @reader-events)))))

(deftest source-retrieval-also-disables-reader-evaluation
  (let [file (java.io.File/createTempFile "beichte-source-boundary-" ".clj")
        fixture (clojure.lang.Var/create)
        before @reader-events]
    (try
      (spit file "#=(beichte.source-boundary-test/reader-hit!)")
      (alter-meta! fixture assoc :file (.getAbsolutePath file) :line 1
                   :ns (the-ns 'beichte.source-boundary-test))
      (binding [*read-eval* true]
        (is (nil? (analyze/source-var fixture))))
      (is (= before @reader-events))
      (finally (.delete file)))))

(deftest invocation-kinds-retain-their-callee-and-operand-effects
  (let [protocol-form '(fn [receiver]
                         (beichte.source-boundary-test/boundary-call receiver 1))
        primitive-form '(beichte.source-boundary-test/primitive-callee 1)]
    (is (some #(= :protocol-invoke (:op %)) (ast/nodes (jvm/analyze protocol-form))))
    (is (some #(= :prim-invoke (:op %)) (ast/nodes (jvm/analyze primitive-form)))))
  (doseq [[expression-api _] apis]
    (is (= :io (level (expression-api '(beichte.source-boundary-test/boundary-call receiver 1)
                                      (context) '#{receiver}))))
    (is (= :io (level (expression-api '(beichte.source-boundary-test/primitive-callee 1)
                                      (context)))))
    (doseq [callee ['beichte.source-boundary-test/boundary-call
                    'beichte.source-boundary-test/primitive-callee]]
      (let [v (find-var callee)
            arguments (if (= callee 'beichte.source-boundary-test/boundary-call)
                        '[receiver 1] '[1])]
        (is (= :io (level (expression-api (cons callee arguments)
                                          (context {v :io}) '#{receiver}))))
        (is (= :pure (level (expression-api (cons callee arguments)
                                            (context {v :pure}) '#{receiver}))))))
    (doseq [expression
            ['(beichte.source-boundary-test/boundary-call
               (do (println "receiver") receiver) 1)
             '(beichte.source-boundary-test/boundary-call
               receiver (do (println "argument") 1))
             '(beichte.source-boundary-test/primitive-callee
               (do (println "argument") 1))]]
      (is (= :io (level (expression-api expression
                                        (context {#'boundary-call :pure #'primitive-callee :pure})
                                        '#{receiver})))))))

(deftest receiver-and-keyword-targets-are-not-discarded
  (doseq [[expression-api _] apis]
    (is (= :pure (level (expression-api '(.length "text")
                                        (context {["java.lang.String" 'length] :pure})))))
    (is (= :pure (level (expression-api '(:key {:key 1}) (context))))))
  (doseq [[expression-api _] apis
          expression ['(.length ^String (do (println "receiver") "text"))
                      '(instance? String (do (println "target") "text"))
                      '(:key (do (println "target") {:key 1}))
                      '(.unknownMember receiver)]]
    (is (= :io (level (expression-api expression
                                      (context {["java.lang.String" 'length] :pure})
                                      '#{receiver})))))
  (is (contains? (:flags (b/analyze-full
                          '(instance? String (do (throw (Exception. "target")) "text"))
                          (context)))
                 :throws)))
