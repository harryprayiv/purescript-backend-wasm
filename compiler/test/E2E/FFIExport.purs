-- | End-to-end coverage of **export-direction** marshalling (ADR 0014): a JS caller
-- | invokes wasm exports with ordinary JS values and gets ordinary JS values back.
-- | `callExportJson` drives each export generically — JSON args in, marshalled to
-- | wasm per the export's param kinds, called, and the result marshalled back out and
-- | JSON-stringified — so the assertions compare plain JSON strings. Covers Int /
-- | Number / Boolean / String / Array / Record in both directions, plus edge cases
-- | (empty string, empty array).
module Test.E2E.FFIExport (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Node.Cbor (decodeFirst)
import Node.FS.Sync (readFile)
import PureScript.ExternsFile (ExternsFile)
import PureScript.ExternsFile.Decoder.Class (decoder)
import PureScript.ExternsFile.Decoder.Monad (runDecoder)
import Test.E2E.Wasm (callExportJson, exportManifestOf, instantiateForeignStr)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

foreign import noImports :: Foreign

decodeExterns :: String -> Aff (Either _ ExternsFile)
decodeExterns path = do
  buf <- liftEffect (readFile path)
  fgn <- decodeFirst buf
  pure (runDecoder decoder fgn)

roots :: Array (Array String)
roots = [ [ "Example", "FFIExport" ] ]

-- | Instantiate the export fixture and run `k` with a generic marshalled-call helper
-- | (`call name argsJson` → result JSON), raising any externs-decode failure.
withExports :: ((String -> String -> Aff String) -> Aff Unit) -> Aff Unit
withExports k =
  decodeExterns "compiler/test/fixtures/Example.FFIExport.externs.cbor" >>= case _ of
    Left err -> fail (show err)
    Right ef -> do
      inst <- liftEffect (instantiateForeignStr [ ef ] noImports roots [ "compiler/test/fixtures/Example.FFIExport.corefn.json" ])
      let manifest = exportManifestOf [ ef ] roots
      k (\name args -> liftEffect (callExportJson inst manifest name args))

spec :: Spec Unit
spec = describe "Test.E2E.FFIExport (wasm export marshalling, ADR 0014)" do
  it "round-trips a String argument and result (incl. empty)" $ withExports \call -> do
    call "echoStr" "[\"hi\"]" >>= (_ `shouldEqual` "\"hi\"")
    call "echoStr" "[\"\"]" >>= (_ `shouldEqual` "\"\"")
  it "marshals a Boolean argument and result (i31ref <-> JS boolean)" $ withExports \call -> do
    call "notB" "[true]" >>= (_ `shouldEqual` "false")
    call "notB" "[false]" >>= (_ `shouldEqual` "true")
  it "round-trips an Array (incl. empty) and a nested-Number Array" $ withExports \call -> do
    call "echoArr" "[[1,2,3]]" >>= (_ `shouldEqual` "[1,2,3]")
    call "echoArr" "[[]]" >>= (_ `shouldEqual` "[]")
    call "echoNums" "[[1.5,2.5]]" >>= (_ `shouldEqual` "[1.5,2.5]")
  it "marshals a Record argument (field projection) and a Record result" $ withExports \call -> do
    call "getX" "[{\"x\":7,\"y\":9}]" >>= (_ `shouldEqual` "7")
    call "mkPoint" "[5]" >>= (_ `shouldEqual` "{\"x\":5,\"y\":5}")
  it "passes a Number through the raw f64 ABI" $ withExports \call ->
    call "idNum" "[2.5]" >>= (_ `shouldEqual` "2.5")
  -- nullary value bindings (CAFs): evaluated with no arguments, the value marshalled
  -- out (the production loader exposes these as values, not functions — ADR 0006 aside)
  it "evaluates a nullary Int value binding" $ withExports \call ->
    call "ultimateAnswer" "[]" >>= (_ `shouldEqual` "42")
  it "evaluates a nullary String value binding (marshalled)" $ withExports \call ->
    call "greeting" "[]" >>= (_ `shouldEqual` "\"hi\"")
