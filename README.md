# beichte

**Static effect inference for Clojure — no type system required.**

Beichte (German: *beichten*, to confess) analyzes Clojure code via
[tools.analyzer.jvm](https://github.com/clojure/tools.analyzer) and infers
side-effect levels on a four-point lattice:

```
:pure < :local < :mutation < :io
```

- **`:pure`** — no side effects. Safe for AD, CSE, GPU compilation, parallelization.
- **`:local`** — thread-local mutation (transients, array writes, volatile). Safe for GPU/SIMD/parallel, not for AD/CSE.
- **`:mutation`** — global mutable state (atoms, refs, vars). Only safe for sequential execution.
- **`:io`** — external I/O (println, file ops, network). Only safe for sequential, cannot reorder.

Designed for compiler builders: validate that code is safe for a compilation
target before transforming it.

## Quick Start

```clojure
;; deps.edn
{:deps {io.replikativ/beichte {:mvn/version "0.2.XXXX"} ;; see Clojars for latest}}
```

```clojure
(require '[beichte.core :as b])

;; Analyze expressions
(b/analyze '(+ 1 2))              ;; => :pure
(b/analyze '(aset arr 0 42))      ;; => :local
(b/analyze '(swap! a inc))        ;; => :mutation
(b/analyze '(println "hi"))       ;; => :io

;; Check if code is safe for a compilation target
(b/compilable? :gpu '(+ 1 2))     ;; => true
(b/compilable? :ad '(swap! a f))  ;; => false

;; Analyze a var
(b/analyze-var #'clojure.core/+)  ;; => :pure
(b/analyze-var #'clojure.core/swap!)  ;; => :mutation

;; Analyze all public vars in a namespace
(b/analyze-ns 'my.lib)            ;; => {#'my.lib/foo :pure, #'my.lib/bar! :io, ...}

;; Filter vars compilable for a target
(b/filter-compilable :gpu (vals (ns-publics 'my.lib)))
```

## Feature Flags

Orthogonal to the state effect lattice, beichte tracks **feature flags** —
properties that specific compilation targets may not support:

```clojure
;; Throws — partial functions, non-local control flow
(b/analyze-full '(throw (Exception. "x")))
;; => {:effect :pure, :flags #{:throws}}

;; Randomness — nondeterministic behavior
(b/analyze-full '(rand))
;; => {:effect :pure, :flags #{:random}}

;; compilable? checks both budget and flags
(b/compilable? :gpu '(throw (Exception. "x")))  ;; => false (GPU can't throw)
(b/compilable? :ad '(throw (Exception. "x")))   ;; => true  (AD tolerates throws)
```

| Flag | Meaning | GPU | AD | CSE |
|------|---------|-----|----|-----|
| `:throws` | May throw exceptions | blocked | OK | OK |
| `:random` | Uses randomness | OK | OK | blocked |
| `:reflects` | Uses reflection | blocked | OK | OK |
| `:allocates` | Heap allocates | info | OK | OK |

Flags propagate via set union through the call graph.

## Context (Caching for Compiler Pipelines)

By default, each `analyze` call creates a fresh analysis — no cross-call
caching, no stale results in the REPL. For compiler pipelines that analyze
many functions, create a context to reuse cached var analysis:

```clojure
;; Create once per compilation pass
(let [ctx (b/make-context)]
  (b/analyze '(foo x) ctx)        ;; analyzes foo from source, caches result
  (b/analyze '(bar (foo x)) ctx)  ;; foo already cached, only bar analyzed
  (b/analyze-var #'baz ctx)       ;; works for vars too
  (b/compilable? :gpu '(qux) ctx) ;; reuses cached results
  )

;; With custom registry
(let [ctx (b/make-context {:registry my-registry})]
  ...)
```

## How It Works

Beichte performs **lightweight abstract interpretation** where the abstract
domain is the effect lattice. It walks the tools.analyzer.jvm AST and:

1. Looks up known effects from a **pre-annotated registry** (~300 entries
   covering `clojure.core`, `java.lang.Math`, Clojure runtime internals)
2. For unknown vars, **reads and analyzes their source** recursively
3. **Propagates** the join (maximum) of sub-expression effects through the
   call graph, with cycle detection

This is analogous to Haskell's IO monad for tracking effects, but as an
external analysis rather than a type system feature. The lattice is finite
and small, so fixpoint computation is trivial.

Higher-order functions like `map` and `filter` are annotated as `:pure`
themselves. When you write `(map println xs)`, the overall effect is
`:io` because beichte joins the effects of the function and all arguments.

## For Compiler Builders

Beichte provides effect budgets for common compilation targets:

| Target | Budget | Meaning |
|--------|--------|---------|
| `:ad` | `:pure` | Automatic differentiation — no effects at all |
| `:cse` | `:pure` | Common subexpression elimination |
| `:gpu` | `:local` | GPU kernels — thread-local scratch OK |
| `:simd` | `:local` | SIMD vectorization |
| `:parallel` | `:local` | Parallel execution |
| `:sequential` | `:io` | Sequential — anything goes |

### Custom Registries

Extend the default registry with domain-specific annotations:

```clojure
(require '[beichte.registry :as reg])

(def my-registry
  (reg/with-entries (reg/default-registry)
    {#'my.math/fast-sin :pure
     #'my.gpu/atomic-add! :local
     #'my.db/query! :io}))

(b/analyze '(my.math/fast-sin x) {:registry my-registry})
;; => :pure
```

### Effect Lattice API

```clojure
(require '[beichte.effect :as eff])

(eff/join :pure :local)          ;; => :local
(eff/join-all [:pure :local :mutation])  ;; => :mutation
(eff/<=effect :local :mutation)  ;; => true
(eff/within-budget? :gpu-budget :local)  ;; => true
(eff/budget-for :gpu)            ;; => :local
```

## Modules

- **`beichte.effect`** — Effect lattice: levels, ordering, join, budgets
- **`beichte.registry`** — Immutable registries, pre-annotated clojure.core and Java
- **`beichte.analyze`** — AST-walking effect inference engine
- **`beichte.core`** — Public API (delegates to the above)

## Backwards Compatibility

The legacy `impure?` / `pure-ground` / `register-pure!` API still works.
New code should use `analyze`, `analyze-var`, and `analyze-ns` instead.

## Requirements

- **JDK 11+**
- **Clojure 1.12+**

## License

Copyright © 2017–2026 Christian Weilbach

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
