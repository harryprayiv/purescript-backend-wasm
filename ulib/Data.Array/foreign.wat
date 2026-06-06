;; ulib: curated wasm FFI for the `Data.Array` source module (ADR 0012).
;;
;; Merged by `bin` (under the module name "Data.Array") when a program imports one of
;; these foreigns, the same way a project-local `foreign.wat` is. Each export speaks the
;; **internal ABI** (eqref / i32 / f64 — no marshalling glue) and the shared GC value
;; types are declared identically to the runtime so `wasm-merge` canonicalises them.
(module
  (type $Vals (array (mut eqref))) ;; an `Array a` value (also a record's value row)

  ;; Data.Array.reverse :: Array a -> Array a
  (func (export "reverse") (param $xs eqref) (result eqref)
    (local $va (ref $Vals))
    (local $n i32)
    (local $i i32)
    (local $out (ref $Vals))
    (local.set $va (ref.cast (ref $Vals) (local.get $xs)))
    (local.set $n (array.len (local.get $va)))
    (local.set $out (array.new $Vals (ref.null none) (local.get $n)))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
        (array.set $Vals (local.get $out) (local.get $i)
          (array.get $Vals (local.get $va) (i32.sub (i32.sub (local.get $n) (i32.const 1)) (local.get $i))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)))
    (local.get $out)))
