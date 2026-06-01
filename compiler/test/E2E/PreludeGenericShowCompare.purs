-- | End-to-end test that the rest of the **`Generic` deriving** family works:
-- | `genericCompare` (`Data.Ord.Generic`) and `genericShow` (`Data.Show.Generic`),
-- | alongside the already-covered `genericEq`.
-- |
-- | `genericCompare` needs nothing beyond the decision-tree matcher and dictionary
-- | dispatch already in place: it folds the generic rep (`Sum`/`Product`/…) down to
-- | an `Ordering`. `genericShow` resolves constructor names through `reflectSymbol`,
-- | which `purs` has already lowered to value-level string literals inside the
-- | synthesised `IsSymbol` dictionaries (`{ reflectSymbol: \_ -> "B" }`); the only
-- | runtime addition is `Data.Show.Generic`'s `intercalate` foreign (joining the
-- | shown arguments), now the `$rt.intercalate` helper.
-- |
-- | Each `show` result is compared *inside* wasm by real Prelude `==` on `String`
-- | (so no string has to cross the host boundary); `compare` results are mapped to
-- | `Int` via a local `Ordering` match.
module Test.E2E.PreludeGenericShowCompare (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x0, instantiateLinked)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Prelude Generic show/compare (e2e): genericCompare + genericShow -> wasm -> run"
    $ before
        ( liftEffect
            ( instantiateLinked [ [ "GenSC" ] ]
                [ "compiler/test/fixtures/GenSC.corefn.json"
                , "compiler/test/fixtures/Data.Generic.Rep.corefn.json"
                , "compiler/test/fixtures/Data.Eq.Generic.corefn.json"
                , "compiler/test/fixtures/Data.Ord.Generic.corefn.json"
                , "compiler/test/fixtures/Data.Show.Generic.corefn.json"
                , "compiler/test/fixtures/Data.Eq.corefn.json"
                , "compiler/test/fixtures/Data.Ord.corefn.json"
                , "compiler/test/fixtures/Data.Ordering.corefn.json"
                , "compiler/test/fixtures/Data.Show.corefn.json"
                , "compiler/test/fixtures/Data.Ring.corefn.json"
                , "compiler/test/fixtures/Data.Semiring.corefn.json"
                , "compiler/test/fixtures/Data.Symbol.corefn.json"
                , "compiler/test/fixtures/Type.Proxy.corefn.json"
                , "compiler/test/fixtures/Data.HeytingAlgebra.corefn.json"
                , "compiler/test/fixtures/Data.Semigroup.corefn.json"
                ]
            )
        )
    $ do
        it "genericCompare orders across and within constructors" \inst -> do
          ab <- liftEffect (callI32x0 inst "cmpAB") -- A < B    => LT (0)
          ba <- liftEffect (callI32x0 inst "cmpBA") -- B > A    => GT (2)
          bblt <- liftEffect (callI32x0 inst "cmpBBlt") -- B 1 < B 2 => LT (0)
          bbeq <- liftEffect (callI32x0 inst "cmpBBeq") -- B 5 = B 5 => EQ (1)
          cclt <- liftEffect (callI32x0 inst "cmpCClt") -- C 1 2 < C 1 3 => LT (0)
          cceq <- liftEffect (callI32x0 inst "cmpCCeq") -- C 7 8 = C 7 8 => EQ (1)
          [ ab, ba, bblt, bbeq, cclt, cceq ] `shouldEqual` [ 0, 2, 0, 1, 0, 1 ]

        it "genericShow renders nullary, single-field, and product constructors" \inst -> do
          a <- liftEffect (callI32x0 inst "showA") -- "A"
          b <- liftEffect (callI32x0 inst "showB") -- "(B 5)"
          c <- liftEffect (callI32x0 inst "showC") -- "(C 1 2)"
          neg <- liftEffect (callI32x0 inst "showNeg") -- "(B -3)"
          [ a, b, c, neg ] `shouldEqual` [ 1, 1, 1, 1 ]
