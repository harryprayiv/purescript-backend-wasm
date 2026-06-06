;; ulib: curated wasm FFI for `Data.Ord` (ADR 0012). Fragment (see ulib/_header.wat).
;; Apply a curried two-argument closure f(x)(y) (two call_ref steps); self-contained over the
;; shared $Clo/$Code header types.
  (func $callClo2 (param $f eqref) (param $x eqref) (param $y eqref) (result eqref)
    (local $c1 (ref $Clo))
    (local $c2 (ref $Clo))
    (local.set $c1 (ref.cast (ref $Clo) (local.get $f)))
    (local.set $c2 (ref.cast (ref $Clo)
      (call_ref $Code (local.get $c1) (local.get $x) (ref.cast (ref $Code) (struct.get $Clo 0 (local.get $c1))))))
    (call_ref $Code (local.get $c2) (local.get $y) (ref.cast (ref $Code) (struct.get $Clo 0 (local.get $c2)))))

;; Data.Ord.ordArrayImpl :: (a -> a -> Int) -> Array a -> Array a -> Int
  (func $arrayOrd (export "ordArrayImpl") (param $f eqref) (param $xs eqref) (param $ys eqref) (result i32)
    (local $va (ref $Vals))
    (local $vb (ref $Vals))
    (local $la i32)
    (local $lb i32)
    (local $i i32)
    (local $o i32)
    (local.set $va (ref.cast (ref $Vals) (local.get $xs)))
    (local.set $vb (ref.cast (ref $Vals) (local.get $ys)))
    (local.set $la (array.len (local.get $va)))
    (local.set $lb (array.len (local.get $vb)))
    (block $done (result i32)
      (loop $loop
        (if (i32.or (i32.ge_u (local.get $i) (local.get $la)) (i32.ge_u (local.get $i) (local.get $lb)))
          (then (br $done
            (if (result i32) (i32.eq (local.get $la) (local.get $lb)) (then (i32.const 0))
              (else (if (result i32) (i32.gt_u (local.get $la) (local.get $lb)) (then (i32.const -1)) (else (i32.const 1))))))))
        (local.set $o (struct.get $Int 0 (ref.cast (ref $Int)
          (call $callClo2 (local.get $f)
            (array.get $Vals (local.get $va) (local.get $i))
            (array.get $Vals (local.get $vb) (local.get $i))))))
        (if (i32.ne (local.get $o) (i32.const 0)) (then (br $done (local.get $o))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop))))
