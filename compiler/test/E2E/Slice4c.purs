-- | End-to-end test of Slice 4c (arrays): an `Array` is the bare
-- | `$Vals = (array (mut eqref))` (the same heap type as ADT fields / record
-- | values), so literals are `array.new_fixed` and the `length`/`index`
-- | intrinsics are `array.len`/`array.get`. Built internally; an `Int` element or
-- | a length crosses the host boundary as `i32`.
module Test.E2E.Slice4c (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x1, instantiateFixture)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Slice 4c (e2e): arrays -> wasm -> run"
    $ before (liftEffect (instantiateFixture "compiler/test/fixtures/Slice4c.corefn.json"))
    $ do
        -- countNums _ = lengthA nums   (nums = [10, 20, 30])
        it "builds an array literal and measures its length" \inst -> do
          result <- liftEffect (callI32x1 inst "countNums" 0)
          result `shouldEqual` 3

        -- nthNum i = indexA nums i
        it "indexes an array" \inst -> do
          a <- liftEffect (callI32x1 inst "nthNum" 0)
          b <- liftEffect (callI32x1 inst "nthNum" 2)
          [ a, b ] `shouldEqual` [ 10, 30 ]

        -- sumFirstTwo _ = addI (indexA nums 0) (indexA nums 1)
        it "indexes twice and combines the (unboxed Int) elements" \inst -> do
          result <- liftEffect (callI32x1 inst "sumFirstTwo" 0)
          result `shouldEqual` 30

        -- cell _ = indexA (indexA grid 1) 0   (grid = [[1,2],[3,4]])
        it "indexes a nested array" \inst -> do
          result <- liftEffect (callI32x1 inst "cell" 0)
          result `shouldEqual` 3
