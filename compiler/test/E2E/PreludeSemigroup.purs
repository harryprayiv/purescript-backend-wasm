-- | End-to-end test of real `Prelude` **`Data.Semigroup`** (`<>` / `append`).
-- | `String` `<>` (`concatString`) reuses the existing `$rt.strConcat` runtime
-- | helper, and `Array` `<>` (`concatArray`) uses the new `$rt.arrayConcat`:
-- | allocate a `$Vals` of the combined length (`array.new_default`) and
-- | `array.copy` both halves in. The string result is checked by equality
-- | (`eqStringImpl` → `$rt.strEq`); the array result is observed with the internal
-- | `lengthA` / `indexA` array intrinsics. `Sgp` is linked with `Data.Semigroup`
-- | (ADR 0009).
module Test.E2E.PreludeSemigroup (spec) where

import Prelude

import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x0, callI32x1, instantiateLinked)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Prelude Semigroup (e2e): <> on String and Array -> wasm -> run"
    $ before
        ( liftEffect
            ( instantiateLinked [ [ "Sgp" ] ]
                [ "compiler/test/fixtures/Sgp.corefn.json"
                , "compiler/test/fixtures/Data.Semigroup.corefn.json"
                , "compiler/test/fixtures/Data.Eq.corefn.json"
                ]
            )
        )
    $ do
        it "concatenates Strings (\"foo\" <> \"bar\" == \"foobar\")" \inst -> do
          ok <- liftEffect (callI32x0 inst "strOk")
          ok `shouldEqual` 1

        it "concatenates Arrays (length of [1,2,3] <> [4,5])" \inst -> do
          n <- liftEffect (callI32x0 inst "arrLen")
          n `shouldEqual` 5

        it "preserves Array element order across the join ([10,20] <> [30,40])" \inst -> do
          xs <- liftEffect (traverse (callI32x1 inst "arrAt") [ 0, 1, 2, 3 ])
          xs `shouldEqual` [ 10, 20, 30, 40 ]
