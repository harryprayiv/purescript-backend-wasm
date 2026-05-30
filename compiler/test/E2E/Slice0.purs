-- | End-to-end test of the Slice 0 pipeline: decode a real `corefn.json`, lower
-- | it to the backend IR, generate a Binaryen module, emit a wasm binary,
-- | instantiate it with the host `WebAssembly` runtime (Node's), and call the
-- | exported functions — proving a pure PureScript module compiles to wasm that
-- | actually runs and computes the right answers.
-- |
-- | The fixture `Slice0.corefn.json` is purs 0.15.16 output for a module that
-- | uses module-local foreign `Int` primitives (mapped to i32 intrinsics) and
-- | saturated calls, so it stays inside the Slice 0 subset.
module Test.E2E.Slice0 where

import Prelude

import Binaryen as B
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import PureScript.Backend.Wasm.Codegen (buildModule)
import PureScript.Backend.Wasm.FromCoreFn (lowerModule)
import PureScript.CoreFn (Module)
import PureScript.CoreFn.FromJSON (decodeModule)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

fixturePath :: String
fixturePath = "compiler/test/fixtures/Slice0.corefn.json"

-- | Parse + decode the fixture, flattening both failure kinds into a message.
decode :: String -> Either String Module
decode source = case jsonParser source of
  Left parseErr -> Left ("parse error: " <> parseErr)
  Right json -> case decodeModule json of
    Left decodeErr -> Left (printJsonDecodeError decodeErr)
    Right m -> Right m

-- | Run the whole pipeline and return a live wasm instance. Any failure
-- | (decode, lowering, validation) is raised as an exception so the test fails
-- | loudly with a useful message.
buildInstance :: Effect Instance
buildInstance = do
  source <- readFixture fixturePath
  m <- case decode source of
    Left err -> throwException (error err)
    Right m -> pure m
  program <- case lowerModule m of
    Left err -> throwException (error ("lowering failed: " <> show err))
    Right program -> pure program
  mod <- buildModule program
  ok <- B.validate mod
  when (not ok) do
    wat <- B.emitText mod
    throwException (error ("module failed validation:\n" <> wat))
  binary <- B.emitBinary mod
  B.dispose mod
  instantiate binary

spec :: Spec Unit
spec =
  describe "Slice 0 (e2e): Slice0.corefn.json -> IR -> wasm -> run"
    $ before (liftEffect buildInstance)
    $ do
        it "double x = addI x x" \inst -> do
          result <- liftEffect (callI32x1 inst "double" 21)
          result `shouldEqual` 42

        it "quad x = double (double x)" \inst -> do
          result <- liftEffect (callI32x1 inst "quad" 21)
          result `shouldEqual` 84

        it "sumOfSquares x y = addI (mulI x x) (mulI y y)" \inst -> do
          result <- liftEffect (callI32x2 inst "sumOfSquares" 3 4)
          result `shouldEqual` 25

        it "five = addI 2 3 (nullary export)" \inst -> do
          result <- liftEffect (callI32x0 inst "five")
          result `shouldEqual` 5

-- | A live `WebAssembly.Instance`.
foreign import data Instance :: Type

foreign import readFixture :: String -> Effect String

-- | Synchronously compile and instantiate a wasm binary (no imports).
foreign import instantiate :: Uint8Array -> Effect Instance

foreign import callI32x0 :: Instance -> String -> Effect Int
foreign import callI32x1 :: Instance -> String -> Int -> Effect Int
foreign import callI32x2 :: Instance -> String -> Int -> Int -> Effect Int

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec
