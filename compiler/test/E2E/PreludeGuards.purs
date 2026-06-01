-- | End-to-end test of **case guards**. A guarded alternative whose pattern
-- | matches may still fail (when none of its guards hold), and matching then
-- | falls through to the subsequent alternatives. The decision-tree compiler
-- | (`Lower.Match`) lowers a guarded leaf to a chain of boolean tests whose final
-- | `else` is the compiled remainder of the matrix.
-- |
-- | `classify` exercises two guards sharing one alternative, then a catch-all;
-- | `unboxPos` / `unboxAny` exercise a guarded constructor pattern (the tag is
-- | switched on first, the guard nested inside the branch, with fallthrough to a
-- | later same-constructor alternative). The guards use real Prelude `>`, so the
-- | module is linked with `Data.Eq` / `Data.Ord` / `Data.Ordering`.
module Test.E2E.PreludeGuards (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x1, instantiateLinked)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Prelude case guards (e2e): guarded alternatives -> wasm -> run"
    $ before
        ( liftEffect
            ( instantiateLinked [ [ "Guards" ] ]
                [ "compiler/test/fixtures/Guards.corefn.json"
                , "compiler/test/fixtures/Data.Eq.corefn.json"
                , "compiler/test/fixtures/Data.Ord.corefn.json"
                , "compiler/test/fixtures/Data.Ordering.corefn.json"
                ]
            )
        )
    $ do
        -- classify n = case n of _ | n > 10 -> 2 | n > 0 -> 1 ; _ -> 0
        it "picks the first satisfied guard, then the catch-all" \inst -> do
          big <- liftEffect (callI32x1 inst "classify" 20)
          small <- liftEffect (callI32x1 inst "classify" 5)
          zero <- liftEffect (callI32x1 inst "classify" 0)
          neg <- liftEffect (callI32x1 inst "classify" (-3))
          [ big, small, zero, neg ] `shouldEqual` [ 2, 1, 0, 0 ]

        -- unboxPos x = unbox (Pos x); unbox: Pos x | x > 0 -> x ; Any x -> x ; Pos _ -> 0
        -- A failing guard on a matched constructor falls through to `Pos _ -> 0`.
        it "falls through a failing guard to a later same-constructor alternative" \inst -> do
          held <- liftEffect (callI32x1 inst "unboxPos" 7)
          failed <- liftEffect (callI32x1 inst "unboxPos" (-2))
          atZero <- liftEffect (callI32x1 inst "unboxPos" 0)
          [ held, failed, atZero ] `shouldEqual` [ 7, 0, 0 ]

        -- unboxAny x = unbox (Any x); the unguarded `Any x -> x` alternative
        it "takes an unguarded constructor alternative directly" \inst -> do
          a <- liftEffect (callI32x1 inst "unboxAny" 9)
          b <- liftEffect (callI32x1 inst "unboxAny" (-4))
          [ a, b ] `shouldEqual` [ 9, -4 ]
