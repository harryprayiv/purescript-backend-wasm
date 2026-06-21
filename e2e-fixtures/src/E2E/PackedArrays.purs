module E2E.PackedArrays where

import Prelude

import Data.Int (round)
import Wasm.F64Array as F
import Wasm.I32Array as I

i32Len :: Int -> Int
i32Len _ =
  let
    a = I.unsafeSet (I.unsafeSet (I.unsafeSet (I.unsafeNew 3) 0 10) 1 20) 2 30
  in
    I.length a

i32At :: Int -> Int
i32At i =
  let
    a = I.unsafeSet (I.unsafeSet (I.unsafeSet (I.unsafeNew 3) 0 10) 1 20) 2 30
  in
    I.unsafeIndex a i

i32Sum :: Int -> Int
i32Sum _ =
  let
    a = I.unsafeSet (I.unsafeSet (I.unsafeSet (I.unsafeNew 3) 0 10) 1 20) 2 30
  in
    I.unsafeIndex a 0 + I.unsafeIndex a 1 + I.unsafeIndex a 2

i32ZeroInit :: Int -> Int
i32ZeroInit _ = I.unsafeIndex (I.unsafeNew 4) 2

f64Mul :: Int -> Int
f64Mul _ =
  let
    a = F.unsafeSet (F.unsafeSet (F.unsafeNew 2) 0 3.0) 1 4.0
  in
    round (F.unsafeIndex a 0 * F.unsafeIndex a 1)

f64Len :: Int -> Int
f64Len _ = F.length (F.unsafeNew 5)

f64ZeroInit :: Int -> Int
f64ZeroInit _ = round (F.unsafeIndex (F.unsafeNew 3) 1)