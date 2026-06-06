-- | End-to-end **Effect-machinery coverage in the fast (e2e) lane** (post-mortem 2026-06-07):
-- | an Effect regression once rode along on red CI for ~10 commits because the only Effect
-- | coverage lived in the slow, separate `test:bin` scripts while the routinely-run e2e/unit
-- | suite had none. This links a consumer (`EffP`) with the real `Effect`/`Effect.Ref` closure
-- | and runs it through the same `optimizeProgram` the bin uses, asserting the effects run.
-- |
-- | `voidTest` is the dedicated guard for the **discarded-effect-drop** bug: a `void (Ref.modify
-- | …)` left an un-reduced apply-redex whose effect `runImpure` misread as pure, so the dead
-- | binding + its effect were dropped (`Purity.runImpure`'s redex-arg rule fixes it; reverting
-- | that fix makes this case fail — verified). `forETest` is general `forE` + cross-module
-- | `Ref.modify` coverage. (The other regression — `Specialize` placing `Effect.Ref.modify$specN`
-- | with a body referencing the consumer, breaking `topoOrder` so `Effect.Ref.new` failed to
-- | inline — only reproduces with the full example closure; its dedicated guards are
-- | `effectPrim.mjs` / `refNative.mjs` in `test:bin`, now run by the `.githooks/pre-push` hook.)
module Test.E2E.EffectPrim (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x0, instantiateLinked)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Effect primitives (e2e): cross-module Ref / forE / void -> wasm -> run"
    $ before
        ( liftEffect
            ( instantiateLinked [ [ "EffP" ] ]
                [ "compiler/test/fixtures/Control.Applicative.corefn.json"
                , "compiler/test/fixtures/Control.Apply.corefn.json"
                , "compiler/test/fixtures/Control.Bind.corefn.json"
                , "compiler/test/fixtures/Control.Category.corefn.json"
                , "compiler/test/fixtures/Control.Monad.corefn.json"
                , "compiler/test/fixtures/Control.Semigroupoid.corefn.json"
                , "compiler/test/fixtures/Data.Boolean.corefn.json"
                , "compiler/test/fixtures/Data.BooleanAlgebra.corefn.json"
                , "compiler/test/fixtures/Data.Bounded.corefn.json"
                , "compiler/test/fixtures/Data.CommutativeRing.corefn.json"
                , "compiler/test/fixtures/Data.DivisionRing.corefn.json"
                , "compiler/test/fixtures/Data.Eq.corefn.json"
                , "compiler/test/fixtures/Data.EuclideanRing.corefn.json"
                , "compiler/test/fixtures/Data.Field.corefn.json"
                , "compiler/test/fixtures/Data.Function.corefn.json"
                , "compiler/test/fixtures/Data.Functor.corefn.json"
                , "compiler/test/fixtures/Data.HeytingAlgebra.corefn.json"
                , "compiler/test/fixtures/Data.Monoid.corefn.json"
                , "compiler/test/fixtures/Data.NaturalTransformation.corefn.json"
                , "compiler/test/fixtures/Data.Ord.corefn.json"
                , "compiler/test/fixtures/Data.Ordering.corefn.json"
                , "compiler/test/fixtures/Data.Ring.corefn.json"
                , "compiler/test/fixtures/Data.Semigroup.corefn.json"
                , "compiler/test/fixtures/Data.Semiring.corefn.json"
                , "compiler/test/fixtures/Data.Show.corefn.json"
                , "compiler/test/fixtures/Data.Symbol.corefn.json"
                , "compiler/test/fixtures/Data.Unit.corefn.json"
                , "compiler/test/fixtures/Data.Void.corefn.json"
                , "compiler/test/fixtures/EffP.corefn.json"
                , "compiler/test/fixtures/Effect.corefn.json"
                , "compiler/test/fixtures/Effect.Ref.corefn.json"
                , "compiler/test/fixtures/Prelude.corefn.json"
                , "compiler/test/fixtures/Record.Unsafe.corefn.json"
                , "compiler/test/fixtures/Type.Proxy.corefn.json"
                ]
            )
        )
    $ do
        -- #1: `void` must NOT drop the wrapped effect — `modify (_ + 5)` runs, cell = 5.
        it "void preserves the discarded effect (acc ends at 5)" \inst -> do
          r <- liftEffect (callI32x0 inst "voidTest")
          r `shouldEqual` 5

        -- #2: `forE 0 5` runs the body (each `Ref.modify (_ + i)`) for i = 0..4 → 0+1+2+3+4 = 10.
        it "forE runs the cross-module Ref.modify body each iteration (sum 0..4 = 10)" \inst -> do
          r <- liftEffect (callI32x0 inst "forETest")
          r `shouldEqual` 10
