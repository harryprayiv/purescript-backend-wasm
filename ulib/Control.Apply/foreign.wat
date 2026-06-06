;; ulib: curated wasm FFI for `Control.Apply` (ADR 0012). Fragment (see ulib/_header.wat).
(import "rt" "applyClo" (func $callClo1 (param eqref eqref) (result eqref)))
;; Control.Apply.arrayApply :: Array (a -> b) -> Array a -> Array b
  (func $arrayApply (export "arrayApply") (param $fs eqref) (param $xs eqref) (result eqref)
    (local $vf (ref $Vals))
    (local $vx (ref $Vals))
    (local $l i32)
    (local $k i32)
    (local $i i32)
    (local $j i32)
    (local $n i32)
    (local $f eqref)
    (local $out (ref $Vals))
    (local.set $vf (ref.cast (ref $Vals) (local.get $fs)))
    (local.set $vx (ref.cast (ref $Vals) (local.get $xs)))
    (local.set $l (array.len (local.get $vf)))
    (local.set $k (array.len (local.get $vx)))
    (local.set $out (array.new $Vals (ref.null none) (i32.mul (local.get $l) (local.get $k))))
    (block $di
      (loop $li
        (br_if $di (i32.ge_u (local.get $i) (local.get $l)))
        (local.set $f (array.get $Vals (local.get $vf) (local.get $i)))
        (local.set $j (i32.const 0))
        (block $dj
          (loop $lj
            (br_if $dj (i32.ge_u (local.get $j) (local.get $k)))
            (array.set $Vals (local.get $out) (local.get $n)
              (call $callClo1 (local.get $f) (array.get $Vals (local.get $vx) (local.get $j))))
            (local.set $n (i32.add (local.get $n) (i32.const 1)))
            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (br $lj)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $li)))
    (local.get $out))
