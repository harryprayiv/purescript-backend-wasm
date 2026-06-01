-- | End-to-end test that the **`Function` (`(->) r`, "Reader") instances** of
-- | `Functor` / `Apply` / `Applicative` / `Bind` / `Monad` work — not just `Array`.
-- | These instances carry **no foreigns**: `map = (<<<)` (composition), `apply f g
-- | x = f x (g x)`, `pure = const`, `bind m f x = f (m x) x`, `Monad` is law-only.
-- | They are pure closure construction + application, so they lower with nothing
-- | beyond the existing closure machinery — including `do`-notation over functions.
-- | `FnInst` is linked with `Data.Functor` / `Control.{Apply,Applicative,Bind,
-- | Semigroupoid}` and `Data.Semiring` (for the `+`/`*` inside the readers).
module Test.E2E.PreludeFnInstance (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x1, instantiateLinked)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Prelude Function instances (e2e): Functor/Apply/Applicative/Monad on (->) r"
    $ before
        ( liftEffect
            ( instantiateLinked [ [ "FnInst" ] ]
                [ "compiler/test/fixtures/FnInst.corefn.json"
                , "compiler/test/fixtures/Data.Functor.corefn.json"
                , "compiler/test/fixtures/Control.Apply.corefn.json"
                , "compiler/test/fixtures/Control.Applicative.corefn.json"
                , "compiler/test/fixtures/Control.Bind.corefn.json"
                , "compiler/test/fixtures/Control.Semigroupoid.corefn.json"
                , "compiler/test/fixtures/Control.Category.corefn.json"
                , "compiler/test/fixtures/Data.Semiring.corefn.json"
                ]
            )
        )
    $ do
        it "Functor: map = composition  ((x*2)+1)" \inst -> do
          r <- liftEffect (callI32x1 inst "fnMap" 3)
          r `shouldEqual` 7

        it "Apply: apply f g x = f x (g x)  (x + x*10 = 11x)" \inst -> do
          r <- liftEffect (callI32x1 inst "fnApply" 3)
          r `shouldEqual` 33

        it "Applicative: pure = const  (42)" \inst -> do
          r <- liftEffect (callI32x1 inst "fnPure" 3)
          r `shouldEqual` 42

        it "Bind: bind m f x = f (m x) x  (2x + x = 3x)" \inst -> do
          r <- liftEffect (callI32x1 inst "fnBind" 3)
          r `shouldEqual` 9

        it "Monad do-notation over functions / Reader  ((x+1)+(x*2) = 3x+1)" \inst -> do
          r <- liftEffect (callI32x1 inst "fnDo" 3)
          r `shouldEqual` 10

        it "Category identity and Semigroupoid >>> on functions" \inst -> do
          i <- liftEffect (callI32x1 inst "fnId" 3)
          c <- liftEffect (callI32x1 inst "fnCompose" 3)
          [ i, c ] `shouldEqual` [ 3, 8 ]
