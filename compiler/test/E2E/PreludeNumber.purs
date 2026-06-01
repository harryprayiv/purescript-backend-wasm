-- | End-to-end test of real `Prelude` **`Number` arithmetic**: `+`/`*`/`-` (via
-- | `semiringNumber`/`ringNumber`) and `/` (via `euclideanRingNumber`) map to the
-- | `numAdd`/`numMul`/`numSub`/`numDiv` foreigns — `f64.add`/`sub`/`mul`/`div` on
-- | the unboxed `$Num`. `Data.Int.toNumber` is `f64.convert_i32_s`, and `==` on
-- | `Number` is `eqNumberImpl` (`f64.eq`). `Num` is linked with the numeric
-- | hierarchy (`Data.Semiring`/`Ring`/`CommutativeRing`/`EuclideanRing`) plus
-- | `Data.Eq` / `Data.Int` (ADR 0009).
module Test.E2E.PreludeNumber (spec) where

import Prelude

import Effect.Class (liftEffect)
import Test.E2E.Wasm (callI32x2, instantiateLinked)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Prelude Number arithmetic (e2e): + * - / via dictionaries -> wasm -> run"
    $ before
        ( liftEffect
            ( instantiateLinked [ [ "Num" ] ]
                [ "compiler/test/fixtures/Num.corefn.json"
                , "compiler/test/fixtures/Data.Semiring.corefn.json"
                , "compiler/test/fixtures/Data.Ring.corefn.json"
                , "compiler/test/fixtures/Data.CommutativeRing.corefn.json"
                , "compiler/test/fixtures/Data.EuclideanRing.corefn.json"
                , "compiler/test/fixtures/Data.Eq.corefn.json"
                , "compiler/test/fixtures/Data.Int.corefn.json"
                ]
            )
        )
    $ do
        it "adds / multiplies / subtracts Numbers (matching the Int result)" \inst -> do
          a <- liftEffect (callI32x2 inst "addOk" 3 4)
          m <- liftEffect (callI32x2 inst "mulOk" 5 6)
          s <- liftEffect (callI32x2 inst "subOk" 9 4)
          [ a, m, s ] `shouldEqual` [ 1, 1, 1 ]

        -- divOk a b = if (a / b) * b == a then 1 else 0
        it "divides Numbers (f64.div, checked by (a/b)*b == a)" \inst -> do
          x <- liftEffect (callI32x2 inst "divOk" 10 4)
          y <- liftEffect (callI32x2 inst "divOk" 6 2)
          [ x, y ] `shouldEqual` [ 1, 1 ]
