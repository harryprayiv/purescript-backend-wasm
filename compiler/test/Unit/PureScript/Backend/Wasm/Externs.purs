-- | Unit tests for the externs → field-representation bridge: that a constructor's
-- | concrete scalar fields (`Int`/`Number`/`Char`) are read out of the externs as
-- | unboxed reps, and everything else stays boxed. Anchored to a real
-- | `externs.cbor` (purs 0.15.16) for `Bench.Main`, whose `IntList`/`Tree`/`TreeQ`
-- | mix concrete-`Int` and recursive fields.
module Test.Unit.PureScript.Backend.Wasm.Externs (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Node.Cbor (decodeFirst)
import Node.FS.Sync (readFile)
import PureScript.Backend.Wasm.Externs (ctorFieldReps)
import PureScript.Backend.Wasm.Lower.IR (Rep(..))
import PureScript.ExternsFile (ExternsFile)
import PureScript.ExternsFile.Decoder.Class (decoder)
import PureScript.ExternsFile.Decoder.Monad (runDecoder)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

decodeExterns :: String -> Aff (Either String ExternsFile)
decodeExterns path = do
  buf <- liftEffect (readFile path)
  fgn <- decodeFirst buf
  pure case runDecoder decoder fgn of
    Left err -> Left (show err)
    Right ef -> Right ef

spec :: Spec Unit
spec = describe "PureScript.Backend.Wasm.Externs (field reps)" do
  it "reads unboxed reps for concrete scalar constructor fields" do
    decodeExterns "compiler/test/fixtures/Bench.Main.externs.cbor" >>= case _ of
      Left err -> fail err
      Right ef -> do
        let reps = ctorFieldReps [ ef ]
        -- Cons Int IntList → [i32, boxed]; Node Int Tree Tree → [i32, boxed, boxed]
        Object.lookup "Bench.Main.Cons" reps `shouldEqual` Just [ I32, Boxed ]
        Object.lookup "Bench.Main.Node" reps `shouldEqual` Just [ I32, Boxed, Boxed ]
        -- QCons Tree TreeQ has no scalar field → all boxed
        Object.lookup "Bench.Main.QCons" reps `shouldEqual` Just [ Boxed, Boxed ]
        -- nullary constructors have no fields
        Object.lookup "Bench.Main.Nil" reps `shouldEqual` Just []
        Object.lookup "Bench.Main.Leaf" reps `shouldEqual` Just []
