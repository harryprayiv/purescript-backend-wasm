-- | End-to-end test of **real `Prelude` arithmetic**: `Arith` uses `+`, `*`, `-`
-- | on `Int`, which desugar through the `Semiring` / `Ring` method accessors and
-- | the `semiringInt` / `ringInt` instance dictionaries (defined in
-- | `Data.Semiring` / `Data.Ring`) down to the `intAdd` / `intMul` / `intSub`
-- | intrinsics. The three modules are linked into one wasm; function-level
-- | reachability lowers only what the arithmetic actually uses, leaving the
-- | modules' many other (unsupported) instances untouched (ADR 0009).
module Test.E2E.PreludeArith (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x2, instantiateLinked)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Prelude arithmetic (e2e): + * - via dictionaries -> wasm -> run"
    $ before
        ( liftEffect
            ( instantiateLinked [ [ "Arith" ] ]
                [ "compiler/test/fixtures/Arith.corefn.json"
                , "compiler/test/fixtures/Data.Semiring.corefn.json"
                , "compiler/test/fixtures/Data.Ring.corefn.json"
                ]
            )
        )
    $ do
        -- sumSquares a b = a * a + b * b   (Semiring: mul, add)
        it "computes a * a + b * b through the Semiring dictionary" \inst -> do
          result <- liftEffect (callI32x2 inst "sumSquares" 3 4)
          result `shouldEqual` 25

        -- diff a b = a - b   (Ring: sub)
        it "computes a - b through the Ring dictionary" \inst -> do
          result <- liftEffect (callI32x2 inst "diff" 10 3)
          result `shouldEqual` 7

        -- poly a b = a * a + b * b - a   (mul, add, sub together)
        it "computes a mixed +/*/- expression" \inst -> do
          a <- liftEffect (callI32x2 inst "poly" 3 4)
          b <- liftEffect (callI32x2 inst "poly" 10 1)
          [ a, b ] `shouldEqual` [ 22, 91 ]
