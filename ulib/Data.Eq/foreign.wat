;; ulib: curated wasm FFI for `Data.Eq` (ADR 0012). Fragment (see ulib/_header.wat).
;; Apply a curried two-argument closure f(x)(y) (two call_ref steps); self-contained over the
;; shared $Clo/$Code header types.
  (func $callClo2 (param $f eqref) (param $x eqref) (param $y eqref) (result eqref)
    (local $c1 (ref $Clo))
    (local $c2 (ref $Clo))
    (local.set $c1 (ref.cast (ref $Clo) (local.get $f)))
    (local.set $c2 (ref.cast (ref $Clo)
      (call_ref $Code (local.get $c1) (local.get $x) (ref.cast (ref $Code) (struct.get $Clo 0 (local.get $c1))))))
    (call_ref $Code (local.get $c2) (local.get $y) (ref.cast (ref $Code) (struct.get $Clo 0 (local.get $c2)))))

;; Data.Eq.eqArrayImpl :: (a -> a -> Boolean) -> Array a -> Array a -> Boolean
(func $arrayEq (export "eqArrayImpl") (param $f eqref) (param $xs eqref) (param $ys eqref) (result eqref)
    (local $va (ref $Vals))
    (local $vb (ref $Vals))
    (local $n i32)
    (local $i i32)
    (local.set $va (ref.cast (ref $Vals) (local.get $xs)))
    (local.set $vb (ref.cast (ref $Vals) (local.get $ys)))
    (local.set $n (array.len (local.get $va)))
    (if (i32.ne (local.get $n) (array.len (local.get $vb))) (then (return (ref.i31 (i32.const 0)))))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
        (if (i32.eqz (i31.get_s (ref.cast i31ref
              (call $callClo2 (local.get $f)
                (array.get $Vals (local.get $va) (local.get $i))
                (array.get $Vals (local.get $vb) (local.get $i))))))
          (then (return (ref.i31 (i32.const 0)))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)))
    (ref.i31 (i32.const 1)))
