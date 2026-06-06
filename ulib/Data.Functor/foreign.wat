;; ulib: curated wasm FFI for `Data.Functor` (ADR 0012). Fragment (see ulib/_header.wat).
(import "rt" "applyClo" (func $callClo1 (param eqref eqref) (result eqref)))
;; Data.Functor.arrayMap :: (a -> b) -> Array a -> Array b
  (func $arrayMap (export "arrayMap") (param $f eqref) (param $xs eqref) (result eqref)
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
          (call $callClo1 (local.get $f) (array.get $Vals (local.get $va) (local.get $i))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)))
    (local.get $out))
