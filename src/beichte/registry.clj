(ns beichte.registry
  "Immutable effect registry for known vars, classes, and methods.

  The registry is a persistent map from entries to effect levels.
  Entries can be:
    - Vars:           #'clojure.core/+
    - Class strings:  \"java.lang.Math\"
    - Method pairs:   [\"java.lang.Math\" 'sin]

  The default registry pre-annotates clojure.core and common Java
  classes. Users create extended registries via `with-entries`."
  (:require [beichte.effect :as effect]))

;; ================================================================
;; Registry construction
;; ================================================================

(defn make-registry
  "Create a registry from a map of {entry -> effect-level}.
  Validates all effect levels."
  [entries]
  (doseq [[k v] entries]
    (effect/level-index v))
  entries)

(defn with-entries
  "Extend a registry with additional entries. Later entries override."
  [registry entries]
  (make-registry (merge registry entries)))

(defn lookup
  "Look up an entry's effect level. Returns nil if unknown."
  [registry entry]
  (get registry entry))

;; ================================================================
;; Clojure core: pre-annotated effect registry
;; ================================================================

(def ^:private core-pure-vars
  "clojure.core vars known to be pure.
  These are either implemented in Java (no source to analyze) or
  are commonly used and worth caching to avoid repeated analysis."
  [;; Arithmetic
   #'clojure.core/+ #'clojure.core/- #'clojure.core/* #'clojure.core//
   #'clojure.core/inc #'clojure.core/dec
   #'clojure.core/min #'clojure.core/max #'clojure.core/abs
   #'clojure.core/quot #'clojure.core/rem #'clojure.core/mod
   #'clojure.core/+' #'clojure.core/-' #'clojure.core/*'
   #'clojure.core/inc' #'clojure.core/dec'

   ;; Unchecked arithmetic
   #'clojure.core/unchecked-add #'clojure.core/unchecked-subtract
   #'clojure.core/unchecked-multiply #'clojure.core/unchecked-inc
   #'clojure.core/unchecked-dec #'clojure.core/unchecked-negate

   ;; Numeric type coercion
   #'clojure.core/byte #'clojure.core/short #'clojure.core/int
   #'clojure.core/long #'clojure.core/float #'clojure.core/double
   #'clojure.core/char #'clojure.core/boolean
   #'clojure.core/num #'clojure.core/bigdec #'clojure.core/bigint #'clojure.core/biginteger
   #'clojure.core/rationalize

   ;; Numeric predicates
   #'clojure.core/zero? #'clojure.core/pos? #'clojure.core/neg?
   #'clojure.core/pos-int? #'clojure.core/neg-int? #'clojure.core/nat-int?
   #'clojure.core/even? #'clojure.core/odd?
   #'clojure.core/number? #'clojure.core/integer? #'clojure.core/float?
   #'clojure.core/rational? #'clojure.core/ratio? #'clojure.core/decimal?
   #'clojure.core/int? #'clojure.core/double?
   #'clojure.core/NaN? #'clojure.core/infinite?

   ;; Comparison
   #'clojure.core/= #'clojure.core/== #'clojure.core/not=
   #'clojure.core/< #'clojure.core/<= #'clojure.core/> #'clojure.core/>=
   #'clojure.core/compare #'clojure.core/identical? #'clojure.core/compare-and-set!

   ;; Boolean/logic
   #'clojure.core/not #'clojure.core/true? #'clojure.core/false?
   #'clojure.core/nil? #'clojure.core/some?

   ;; Strings
   #'clojure.core/str #'clojure.core/subs #'clojure.core/name #'clojure.core/namespace
   #'clojure.core/string? #'clojure.core/char? #'clojure.core/format
   #'clojure.core/symbol #'clojure.core/keyword
   #'clojure.core/symbol? #'clojure.core/keyword?
   #'clojure.core/simple-symbol? #'clojure.core/qualified-symbol?
   #'clojure.core/simple-keyword? #'clojure.core/qualified-keyword?
   #'clojure.core/simple-ident? #'clojure.core/qualified-ident?
   #'clojure.core/ident?

   ;; Sequences (constructors/accessors — lazy but pure)
   #'clojure.core/cons #'clojure.core/first #'clojure.core/rest #'clojure.core/next
   #'clojure.core/ffirst #'clojure.core/fnext #'clojure.core/nfirst #'clojure.core/nnext
   #'clojure.core/second #'clojure.core/last #'clojure.core/butlast
   #'clojure.core/seq #'clojure.core/sequence #'clojure.core/seq?
   #'clojure.core/count #'clojure.core/empty? #'clojure.core/not-empty
   #'clojure.core/nth #'clojure.core/peek
   #'clojure.core/conj #'clojure.core/into #'clojure.core/concat
   #'clojure.core/flatten #'clojure.core/distinct #'clojure.core/dedupe
   #'clojure.core/interleave #'clojure.core/interpose
   #'clojure.core/reverse #'clojure.core/sort #'clojure.core/sort-by
   #'clojure.core/shuffle ;; NOTE: shuffle uses randomness but returns a new vec

   ;; Higher-order (pure in themselves; overall purity depends on fn arg)
   #'clojure.core/map #'clojure.core/mapv #'clojure.core/map-indexed
   #'clojure.core/filter #'clojure.core/filterv #'clojure.core/remove
   #'clojure.core/reduce #'clojure.core/reduce-kv #'clojure.core/reductions
   #'clojure.core/apply #'clojure.core/comp #'clojure.core/partial
   #'clojure.core/juxt #'clojure.core/complement #'clojure.core/fnil
   #'clojure.core/constantly #'clojure.core/identity #'clojure.core/memoize
   #'clojure.core/some #'clojure.core/every? #'clojure.core/not-every? #'clojure.core/not-any?
   #'clojure.core/keep #'clojure.core/keep-indexed
   #'clojure.core/take #'clojure.core/take-while #'clojure.core/take-nth #'clojure.core/take-last
   #'clojure.core/drop #'clojure.core/drop-while #'clojure.core/drop-last
   #'clojure.core/partition #'clojure.core/partition-by #'clojure.core/partition-all
   #'clojure.core/group-by #'clojure.core/frequencies
   #'clojure.core/zipmap #'clojure.core/into-array #'clojure.core/to-array #'clojure.core/to-array-2d

   ;; Ranges and repetition
   #'clojure.core/range #'clojure.core/repeat #'clojure.core/repeatedly
   #'clojure.core/iterate #'clojure.core/cycle

   ;; Collections
   #'clojure.core/vec #'clojure.core/vector #'clojure.core/vector?
   #'clojure.core/subvec #'clojure.core/rseq
   #'clojure.core/list #'clojure.core/list* #'clojure.core/list?
   #'clojure.core/hash-map #'clojure.core/array-map #'clojure.core/sorted-map
   #'clojure.core/sorted-map-by #'clojure.core/zipmap
   #'clojure.core/hash-set #'clojure.core/set #'clojure.core/sorted-set
   #'clojure.core/sorted-set-by #'clojure.core/set?
   #'clojure.core/map? #'clojure.core/coll? #'clojure.core/sequential?
   #'clojure.core/associative? #'clojure.core/counted? #'clojure.core/reversible?
   #'clojure.core/seqable? #'clojure.core/indexed?
   #'clojure.core/empty

   ;; Associative operations
   #'clojure.core/assoc #'clojure.core/assoc-in
   #'clojure.core/dissoc #'clojure.core/disj
   #'clojure.core/get #'clojure.core/get-in
   #'clojure.core/contains? #'clojure.core/find
   #'clojure.core/keys #'clojure.core/vals
   #'clojure.core/select-keys #'clojure.core/update #'clojure.core/update-in
   #'clojure.core/merge #'clojure.core/merge-with
   #'clojure.core/key #'clojure.core/val
   #'clojure.core/map-entry? #'clojure.core/record?

   ;; Array reads (reads are pure; writes are :local)
   #'clojure.core/aget #'clojure.core/alength #'clojure.core/aclone

   ;; Type checks
   #'clojure.core/type #'clojure.core/class #'clojure.core/instance?
   #'clojure.core/isa? #'clojure.core/satisfies?
   #'clojure.core/fn? #'clojure.core/ifn?
   #'clojure.core/tagged-literal? #'clojure.core/uri?
   #'clojure.core/uuid? #'clojure.core/inst?
   #'clojure.core/bytes? #'clojure.core/boolean?

   ;; Metadata (read-only)
   #'clojure.core/meta #'clojure.core/with-meta #'clojure.core/vary-meta

   ;; Hashing
   #'clojure.core/hash #'clojure.core/hash-ordered-coll
   #'clojure.core/hash-unordered-coll #'clojure.core/mix-collection-hash

   ;; Misc pure
   #'clojure.core/pr-str #'clojure.core/prn-str #'clojure.core/print-str
   #'clojure.core/with-out-str ;; captures output into string — pure result
   #'clojure.core/re-pattern #'clojure.core/re-matches #'clojure.core/re-find
   #'clojure.core/re-seq #'clojure.core/re-groups

   ;; Dynamic var reads (we treat as pure — the value is contextual but reading is safe)
   #'clojure.core/*print-readably* #'clojure.core/*print-dup*
   #'clojure.core/*print-length* #'clojure.core/*print-level*
   #'clojure.core/*print-meta* #'clojure.core/*print-namespace-maps*

   ;; Spread/list* internals (used by apply, need to break recursion)
   #'clojure.core/spread
   #'clojure.core/pr])

(def ^:private core-local-vars
  "clojure.core vars that perform local mutation only.
  Safe for parallelization but not for AD/CSE."
  [;; Array writes (mutate the array argument)
   #'clojure.core/aset
   #'clojure.core/aset-int #'clojure.core/aset-long
   #'clojure.core/aset-float #'clojure.core/aset-double
   #'clojure.core/aset-byte #'clojure.core/aset-short
   #'clojure.core/aset-char #'clojure.core/aset-boolean

   ;; Array construction (allocate + fill — local mutation during construction)
   #'clojure.core/make-array #'clojure.core/object-array
   #'clojure.core/int-array #'clojure.core/long-array
   #'clojure.core/float-array #'clojure.core/double-array
   #'clojure.core/byte-array #'clojure.core/short-array
   #'clojure.core/char-array #'clojure.core/boolean-array
   #'clojure.core/booleans #'clojure.core/bytes
   #'clojure.core/chars #'clojure.core/shorts
   #'clojure.core/ints #'clojure.core/longs
   #'clojure.core/floats #'clojure.core/doubles

   ;; Transients (local mutation by design)
   #'clojure.core/transient #'clojure.core/persistent!
   #'clojure.core/conj! #'clojure.core/assoc! #'clojure.core/dissoc!
   #'clojure.core/pop! #'clojure.core/disj!

   ;; Volatile (thread-local mutable box)
   #'clojure.core/volatile! #'clojure.core/vreset! #'clojure.core/vswap!])

(def ^:private core-mutation-vars
  "clojure.core vars that mutate global state."
  [;; Atoms
   #'clojure.core/atom #'clojure.core/swap! #'clojure.core/reset!
   #'clojure.core/swap-vals! #'clojure.core/reset-vals!
   #'clojure.core/compare-and-set!

   ;; Refs
   #'clojure.core/ref #'clojure.core/ref-set #'clojure.core/alter
   #'clojure.core/commute #'clojure.core/ensure #'clojure.core/dosync

   ;; Agents
   #'clojure.core/agent #'clojure.core/send #'clojure.core/send-off
   #'clojure.core/restart-agent #'clojure.core/set-error-handler!
   #'clojure.core/set-error-mode!

   ;; Var mutation
   #'clojure.core/alter-var-root
   #'clojure.core/push-thread-bindings #'clojure.core/pop-thread-bindings

   ;; Deref (reads mutable state — conceptually a read side-effect)
   #'clojure.core/deref #'clojure.core/realized?])

(def ^:private core-io-vars
  "clojure.core vars that perform I/O."
  [#'clojure.core/println #'clojure.core/print
   #'clojure.core/prn #'clojure.core/pr
   #'clojure.core/printf #'clojure.core/newline #'clojure.core/flush
   #'clojure.core/read #'clojure.core/read-string
   #'clojure.core/slurp #'clojure.core/spit
   #'clojure.core/*out* #'clojure.core/*in* #'clojure.core/*err*])

;; ================================================================
;; Java methods: pre-annotated
;; ================================================================

(def ^:private java-pure-methods
  "Java static/instance methods known to be pure."
  {;; Math
   ["java.lang.Math" 'abs] :pure
   ["java.lang.Math" 'sin] :pure
   ["java.lang.Math" 'cos] :pure
   ["java.lang.Math" 'tan] :pure
   ["java.lang.Math" 'asin] :pure
   ["java.lang.Math" 'acos] :pure
   ["java.lang.Math" 'atan] :pure
   ["java.lang.Math" 'atan2] :pure
   ["java.lang.Math" 'exp] :pure
   ["java.lang.Math" 'log] :pure
   ["java.lang.Math" 'log10] :pure
   ["java.lang.Math" 'sqrt] :pure
   ["java.lang.Math" 'cbrt] :pure
   ["java.lang.Math" 'pow] :pure
   ["java.lang.Math" 'ceil] :pure
   ["java.lang.Math" 'floor] :pure
   ["java.lang.Math" 'round] :pure
   ["java.lang.Math" 'rint] :pure
   ["java.lang.Math" 'max] :pure
   ["java.lang.Math" 'min] :pure
   ["java.lang.Math" 'fma] :pure
   ["java.lang.Math" 'toRadians] :pure
   ["java.lang.Math" 'toDegrees] :pure
   ["java.lang.Math" 'signum] :pure
   ["java.lang.Math" 'copySign] :pure
   ["java.lang.Math" 'sinh] :pure
   ["java.lang.Math" 'cosh] :pure
   ["java.lang.Math" 'tanh] :pure
   ["java.lang.StrictMath" 'abs] :pure
   ["java.lang.StrictMath" 'sin] :pure
   ["java.lang.StrictMath" 'cos] :pure
   ["java.lang.StrictMath" 'sqrt] :pure
   ["java.lang.StrictMath" 'pow] :pure
   ["java.lang.StrictMath" 'exp] :pure
   ["java.lang.StrictMath" 'log] :pure

   ;; Clojure Numbers
   ["clojure.lang.Numbers" 'add] :pure
   ["clojure.lang.Numbers" 'minus] :pure
   ["clojure.lang.Numbers" 'multiply] :pure
   ["clojure.lang.Numbers" 'divide] :pure
   ["clojure.lang.Numbers" 'inc] :pure
   ["clojure.lang.Numbers" 'dec] :pure
   ["clojure.lang.Numbers" 'negate] :pure
   ["clojure.lang.Numbers" 'lt] :pure
   ["clojure.lang.Numbers" 'lte] :pure
   ["clojure.lang.Numbers" 'gt] :pure
   ["clojure.lang.Numbers" 'gte] :pure
   ["clojure.lang.Numbers" 'equiv] :pure
   ["clojure.lang.Numbers" 'isPos] :pure
   ["clojure.lang.Numbers" 'isNeg] :pure
   ["clojure.lang.Numbers" 'isZero] :pure
   ["clojure.lang.Numbers" 'unchecked_add] :pure
   ["clojure.lang.Numbers" 'unchecked_minus] :pure
   ["clojure.lang.Numbers" 'unchecked_multiply] :pure
   ["clojure.lang.Numbers" 'unchecked_inc] :pure
   ["clojure.lang.Numbers" 'unchecked_dec] :pure
   ["clojure.lang.Numbers" 'unchecked_negate] :pure
   ["clojure.lang.Numbers" 'shiftLeft] :pure
   ["clojure.lang.Numbers" 'shiftRight] :pure
   ["clojure.lang.Numbers" 'unsignedShiftRight] :pure
   ["clojure.lang.Numbers" 'and] :pure
   ["clojure.lang.Numbers" 'or] :pure
   ["clojure.lang.Numbers" 'xor] :pure
   ["clojure.lang.Numbers" 'not] :pure
   ["clojure.lang.Numbers" 'remainder] :pure
   ["clojure.lang.Numbers" 'quotient] :pure
   ["clojure.lang.Numbers" 'num] :pure
   ["clojure.lang.Numbers" 'int_array] :local
   ["clojure.lang.Numbers" 'long_array] :local
   ["clojure.lang.Numbers" 'float_array] :local
   ["clojure.lang.Numbers" 'double_array] :local
   ["clojure.lang.Numbers" 'byte_array] :local
   ["clojure.lang.Numbers" 'short_array] :local
   ["clojure.lang.Numbers" 'char_array] :local
   ["clojure.lang.Numbers" 'boolean_array] :local

   ;; Clojure RT
   ["clojure.lang.RT" 'seq] :pure
   ["clojure.lang.RT" 'count] :pure
   ["clojure.lang.RT" 'nth] :pure
   ["clojure.lang.RT" 'get] :pure
   ["clojure.lang.RT" 'first] :pure
   ["clojure.lang.RT" 'next] :pure
   ["clojure.lang.RT" 'more] :pure
   ["clojure.lang.RT" 'conj] :pure
   ["clojure.lang.RT" 'assoc] :pure
   ["clojure.lang.RT" 'dissoc] :pure
   ["clojure.lang.RT" 'contains] :pure
   ["clojure.lang.RT" 'find] :pure
   ["clojure.lang.RT" 'keys] :pure
   ["clojure.lang.RT" 'vals] :pure
   ["clojure.lang.RT" 'intCast] :pure
   ["clojure.lang.RT" 'longCast] :pure
   ["clojure.lang.RT" 'doubleCast] :pure
   ["clojure.lang.RT" 'floatCast] :pure
   ["clojure.lang.RT" 'booleanCast] :pure
   ["clojure.lang.RT" 'byteCast] :pure
   ["clojure.lang.RT" 'shortCast] :pure
   ["clojure.lang.RT" 'charCast] :pure
   ["clojure.lang.RT" 'box] :pure
   ["clojure.lang.RT" 'classForName] :pure
   ["clojure.lang.RT" 'classForNameNonLoading] :pure

   ;; Clojure RT — array operations
   ["clojure.lang.RT" 'aget] :pure
   ["clojure.lang.RT" 'alength] :pure
   ["clojure.lang.RT" 'aclone] :pure
   ["clojure.lang.RT" 'aset] :local  ;; array mutation = local

   ;; Clojure Util
   ["clojure.lang.Util" 'identical] :pure
   ["clojure.lang.Util" 'equiv] :pure
   ["clojure.lang.Util" 'hash] :pure
   ["clojure.lang.Util" 'hasheq] :pure
   ["clojure.lang.Util" 'compare] :pure

   ;; Clojure persistent data structure internals
   ["clojure.lang.PersistentHashMap" 'create] :pure
   ["clojure.lang.PersistentVector" 'create] :pure
   ["clojure.lang.LazySeq" 'seq] :pure
   ["clojure.lang.ChunkBuffer" 'add] :local
   ["clojure.lang.ChunkBuffer" 'chunk] :pure
   ["clojure.lang.IChunkedSeq" 'chunkedFirst] :pure
   ["clojure.lang.IChunkedSeq" 'chunkedMore] :pure
   ["clojure.lang.Indexed" 'nth] :pure
   ["clojure.lang.IFn" 'applyTo] :pure
   ["clojure.lang.Var" 'pushThreadBindings] :mutation
   ["clojure.lang.Var" 'popThreadBindings] :mutation

   ;; String methods
   ["java.lang.String" 'length] :pure
   ["java.lang.String" 'charAt] :pure
   ["java.lang.String" 'substring] :pure
   ["java.lang.String" 'indexOf] :pure
   ["java.lang.String" 'lastIndexOf] :pure
   ["java.lang.String" 'startsWith] :pure
   ["java.lang.String" 'endsWith] :pure
   ["java.lang.String" 'contains] :pure
   ["java.lang.String" 'replace] :pure
   ["java.lang.String" 'trim] :pure
   ["java.lang.String" 'toLowerCase] :pure
   ["java.lang.String" 'toUpperCase] :pure
   ["java.lang.String" 'equals] :pure
   ["java.lang.String" 'hashCode] :pure
   ["java.lang.String" 'compareTo] :pure
   ["java.lang.String" 'toCharArray] :pure
   ["java.lang.String" 'valueOf] :pure
   ["java.lang.String" 'format] :pure
   ["java.lang.String" 'concat] :pure
   ["java.lang.String" 'isEmpty] :pure
   ["java.lang.String" 'matches] :pure
   ["java.lang.String" 'split] :pure
   ["java.lang.String" 'join] :pure

   ;; Number boxing
   ["java.lang.Integer" 'valueOf] :pure
   ["java.lang.Integer" 'parseInt] :pure
   ["java.lang.Integer" 'intValue] :pure
   ["java.lang.Long" 'valueOf] :pure
   ["java.lang.Long" 'parseLong] :pure
   ["java.lang.Long" 'longValue] :pure
   ["java.lang.Float" 'valueOf] :pure
   ["java.lang.Float" 'parseFloat] :pure
   ["java.lang.Float" 'floatValue] :pure
   ["java.lang.Float" 'isNaN] :pure
   ["java.lang.Float" 'isInfinite] :pure
   ["java.lang.Double" 'valueOf] :pure
   ["java.lang.Double" 'parseDouble] :pure
   ["java.lang.Double" 'doubleValue] :pure
   ["java.lang.Double" 'isNaN] :pure
   ["java.lang.Double" 'isInfinite] :pure
   ["java.lang.Boolean" 'valueOf] :pure})

(def ^:private java-pure-classes
  "Java classes whose instantiation is known to be pure."
  {"java.lang.Math" :pure
   "java.lang.StrictMath" :pure
   "java.lang.Double" :pure
   "java.lang.Float" :pure
   "java.lang.Long" :pure
   "java.lang.Integer" :pure
   "java.lang.Short" :pure
   "java.lang.Byte" :pure
   "java.lang.Number" :pure
   "java.lang.Boolean" :pure
   "java.lang.Character" :pure
   "java.lang.String" :pure
   "Exception" :pure
   "RuntimeException" :pure
   "IllegalArgumentException" :pure
   "IllegalStateException" :pure
   "UnsupportedOperationException" :pure
   "ArithmeticException" :pure
   "IndexOutOfBoundsException" :pure
   "NullPointerException" :pure
   "ClassCastException" :pure
   "clojure.lang.LazySeq" :pure
   "clojure.lang.ChunkBuffer" :local  ;; mutable during construction
   "clojure.lang.ChunkedCons" :pure
   "clojure.lang.PersistentVector" :pure
   "clojure.lang.PersistentHashMap" :pure
   "clojure.lang.PersistentHashSet" :pure
   "clojure.lang.PersistentArrayMap" :pure
   "clojure.lang.PersistentList" :pure
   "clojure.lang.MapEntry" :pure
   "clojure.lang.Keyword" :pure
   "clojure.lang.Symbol" :pure})

;; ================================================================
;; Composite default registry
;; ================================================================

(defn default-registry
  "Create the default registry with pre-annotated clojure.core and Java entries."
  []
  (make-registry
   (merge
      ;; Clojure core vars
    (zipmap core-pure-vars (repeat :pure))
    (zipmap core-local-vars (repeat :local))
    (zipmap core-mutation-vars (repeat :mutation))
    (zipmap core-io-vars (repeat :io))
      ;; Java methods and classes
    java-pure-methods
    java-pure-classes)))
