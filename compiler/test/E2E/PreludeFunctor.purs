-- | End-to-end test of real `Prelude` **`Functor` / `Apply` / `Bind`** on `Array`:
-- | `map` / `<$>` (`arrayMap`), `<*>` (`arrayApply`), and `>>=` (`arrayBind`). These
-- | higher-order foreigns apply the element closure from the runtime (`$callClo1`)
-- | and build a new `$Vals`; `arrayBind` does it in two passes (sum lengths, then
-- | copy). Results are compared with the (now wired) `Array` `Eq`. `Fab` is linked
-- | with `Data.Functor` / `Control.Apply` / `Control.Bind` plus the `Eq`/`Ord` and
-- | `Semiring`/`Ring` modules the comparisons and `+`/`*` need (ADR 0009).
module Test.E2E.PreludeFunctor (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x0, instantiateLinked)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Prelude Functor/Apply/Bind (e2e): map / <*> / >>= on Array -> wasm -> run"
    $ before
        ( liftEffect
            ( instantiateLinked [ [ "Fab" ] ]
                [ "compiler/test/fixtures/Fab.corefn.json"
                , "compiler/test/fixtures/Data.Functor.corefn.json"
                , "compiler/test/fixtures/Control.Apply.corefn.json"
                , "compiler/test/fixtures/Control.Bind.corefn.json"
                , "compiler/test/fixtures/Data.Eq.corefn.json"
                , "compiler/test/fixtures/Data.Ord.corefn.json"
                , "compiler/test/fixtures/Data.Ordering.corefn.json"
                , "compiler/test/fixtures/Data.HeytingAlgebra.corefn.json"
                , "compiler/test/fixtures/Data.Semiring.corefn.json"
                , "compiler/test/fixtures/Data.Ring.corefn.json"
                ]
            )
        )
    $ do
        it "maps over an Array (map and <$>, incl. empty)" \inst -> do
          m <- liftEffect (callI32x0 inst "mapOk")
          fm <- liftEffect (callI32x0 inst "fmapOk")
          e <- liftEffect (callI32x0 inst "mapEmpty")
          [ m, fm, e ] `shouldEqual` [ 1, 1, 1 ]

        it "applies an Array of functions ((+) <$> [1,2] <*> [10,20])" \inst -> do
          a <- liftEffect (callI32x0 inst "applyOk")
          a `shouldEqual` 1

        it "binds (flatMap) an Array (incl. an empty result)" \inst -> do
          b <- liftEffect (callI32x0 inst "bindOk")
          e <- liftEffect (callI32x0 inst "bindEmpty")
          [ b, e ] `shouldEqual` [ 1, 1 ]
