-- | End-to-end test that **`do`-notation** on the `Array` monad works — confirming
-- | that `Applicative`/`Monad` need nothing beyond what `Bind` already provides.
-- | `do` is desugared (before CoreFn) to nested `bind` + `pure`; `pure x = [x]` is a
-- | singleton array literal, and `Monad`/`Applicative` add no foreigns. So a
-- | `do`-block reduces to `arrayBind` calls plus array construction. Results are
-- | checked with `Array` `Eq`. `Mnd` is linked with the `Bind`/`Apply`/`Applicative`
-- | /`Functor` modules plus the `Eq`/`Ord` and `Semiring` modules (ADR 0009).
module Test.E2E.PreludeMonad (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x0, instantiateLinked)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Prelude do-notation (e2e): Array monad -> wasm -> run"
    $ before
        ( liftEffect
            ( instantiateLinked [ [ "Mnd" ] ]
                [ "compiler/test/fixtures/Mnd.corefn.json"
                , "compiler/test/fixtures/Data.Functor.corefn.json"
                , "compiler/test/fixtures/Control.Apply.corefn.json"
                , "compiler/test/fixtures/Control.Applicative.corefn.json"
                , "compiler/test/fixtures/Control.Bind.corefn.json"
                , "compiler/test/fixtures/Data.Eq.corefn.json"
                , "compiler/test/fixtures/Data.Ord.corefn.json"
                , "compiler/test/fixtures/Data.Ordering.corefn.json"
                , "compiler/test/fixtures/Data.HeytingAlgebra.corefn.json"
                , "compiler/test/fixtures/Data.Semiring.corefn.json"
                ]
            )
        )
    $ do
        it "a do-block desugars to nested bind + pure ([x+y | x<-…, y<-…])" \inst -> do
          ok <- liftEffect (callI32x0 inst "pairsOk")
          ok `shouldEqual` 1

        it "pure is a singleton array" \inst -> do
          ok <- liftEffect (callI32x0 inst "pureOk")
          ok `shouldEqual` 1

        it "a wildcard bind (_ <-) replicates the continuation" \inst -> do
          ok <- liftEffect (callI32x0 inst "replOk")
          ok `shouldEqual` 1
