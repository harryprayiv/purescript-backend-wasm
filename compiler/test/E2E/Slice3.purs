-- | End-to-end test of the Slice 3 pipeline (records + type-class dictionaries):
-- | dictionaries are records (label-id-keyed; ADR 0001 / 0007), method dispatch
-- | is a runtime label search (`$rt.proj`), instances are CAFs referenced by
-- | value, and superclass access reads a thunked field and applies it. The
-- | fixtures expose `i32 -> i32` entry points so the whole path runs as wasm.
module Test.E2E.Slice3 (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x0, callI32x1, instantiateFixture)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Slice 3 (e2e): dictionary-passing -> wasm -> run"
    $ before (liftEffect (instantiateFixture "compiler/test/fixtures/Slice3.corefn.json"))
    $ do
        -- doubleInt n = double n, double x = plus x x (Addable Int, plus = addI):
        -- builds the dictionary, projects `plus`, dispatches it twice.
        it "dispatches a class method through a passed dictionary" \inst -> do
          result <- liftEffect (callI32x1 inst "doubleInt" 21)
          result `shouldEqual` 42

        -- sumNil = plus nil nil: the nullary method `nil` (= 0) projected from the
        -- instance dictionary (a CAF), then `plus` dispatched on it.
        it "projects a nullary method and a method from an instance CAF" \inst -> do
          result <- liftEffect (callI32x0 inst "sumNil")
          result `shouldEqual` 0

  describe "Slice 3 (e2e): superclass access -> wasm -> run"
    $ before (liftEffect (instantiateFixture "compiler/test/fixtures/Slice3b.corefn.json"))
    $ do
        -- viaDerivedOf n = useBaseViaDerived n, calling a Base method under a
        -- Derived constraint: one superclass hop (read the thunked `Base0` field
        -- and apply it) before the method projection.
        it "reaches a one-level superclass dictionary" \inst -> do
          result <- liftEffect (callI32x1 inst "viaDerivedOf" 7)
          result `shouldEqual` 7

        -- viaTopOf n = useBaseViaTop n: two superclass hops (Top -> Derived ->
        -- Base), each a thunked-field read + application, nested.
        it "reaches a two-level superclass dictionary" \inst -> do
          result <- liftEffect (callI32x1 inst "viaTopOf" 42)
          result `shouldEqual` 42
