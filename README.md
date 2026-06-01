# PureScript Backend Wasm

An experimental WebAssembly backend for PureScript compiler

[![purs - v0.15.16](https://img.shields.io/badge/purs-v0.15.16-blue?logo=purescript)](https://github.com/purescript/purescript/releases/tag/v0.15.15) [![CI](https://github.com/katsujukou/purescript-backend-wasm/actions/workflows/ci.yaml/badge.svg)](https://github.com/katsujukou/purescript-backend-wasm/actions/workflows/ci.yaml)

## Overview

The compiler consumes `purs`'s CoreFn (`corefn.json`) and externs (`externs.cbor`)
output and produces a single WebAssembly module via the
[Binaryen](https://github.com/WebAssembly/binaryen) JS API. It targets **Wasm
GC**, so heap values (ADTs, records, closures) are reclaimed by the host VM.

Key architectural decisions are recorded as ADRs under
[`docs/design-decisions/`](docs/design-decisions/).

## Roadmap

Near-term milestone: **compile PureScript modules that depend only on `Prelude`
to a single wasm module.** `Effect` and other effectful computations come later.

Front end:

- [x] CoreFn decoder (`corefn.json` → AST, purs 0.15.16)
- [x] Externs decoder (`externs.cbor` → AST)
- [x] Binaryen FFI bindings (low-level, growing per slice)

Code generation, by slice (see
[ADR 0001](docs/design-decisions/0001-wasm-gc-substrate-and-value-representation.md) /
[ADR 0003](docs/design-decisions/0003-intermediate-ir.md)):

- [x] **Slice 0** — scalar core: top-level functions, saturated calls, integer
      literals, inlined i32 intrinsics; exported and runnable from the host
- [x] **Slice 1** — boxing (`eqref`) + ADTs + pattern matching (decision trees)
- [x] **Slice 2** — closures + currying (eval/apply): partial/over application,
      first-class functions, top-level & local recursion (`let rec` knot-tying)
- [x] **Slice 3** — type-class dictionaries (dictionary-passing E2E): dictionaries
      as label-id-keyed records, method dispatch by runtime label search, instance
      CAFs, superclass access. General extensible records deferred (ADR 0007);
      positional/tuple dictionary specialization is a later optimization (ADR 0007)
- [x] **Slice 4** — scalar literals (`Char`/`Number`/`Boolean`, `i31`/`f64`) +
      literal-pattern matching (`if`, `case n of 0 ->`); strings (UTF-8 `$Str`,
      concat/length/equality runtime helpers, string patterns); arrays (`$Vals`
      literals, length/index). `show` and higher-order array functions deferred

Later:

- [ ] Real `Prelude` arithmetic via dictionaries (`+`, `*`, …) end to end
- [ ] `Effect` and effectful computation
- [ ] Optimizations: unboxing, arity raising / uncurrying, nominal record layout,
      unboxed/immediate enum constructors (OCaml-style constant constructors)
- [ ] User-defined FFI (beyond the built-in intrinsics table)