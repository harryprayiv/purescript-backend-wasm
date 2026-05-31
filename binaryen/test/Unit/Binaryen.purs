module Test.Unit.Binaryen where

import Prelude

import Binaryen as B
import Data.ArrayBuffer.Types (Uint8Array)
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Fixtures (buildAddInto, withModule)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

spec :: Spec Unit
spec = describe "Binaryen (unit)" do
  describe "module lifecycle" do
    it "validates a freshly created empty module" do
      ok <- liftEffect $ withModule B.validate
      ok `shouldEqual` true

  describe "function building" do
    it "builds a valid add(i32, i32) -> i32 function" do
      { ok } <- liftEffect buildAdd
      ok `shouldEqual` true

    it "emits WAT with the expected signature and body" do
      { wat } <- liftEffect buildAdd
      -- the function and its (i32, i32) -> i32 signature
      wat `shouldSatisfy` String.contains (Pattern "(func $add")
      wat `shouldSatisfy` String.contains (Pattern "(param $0 i32) (param $1 i32)")
      wat `shouldSatisfy` String.contains (Pattern "(result i32)")
      -- the body built from localGet + i32Add
      wat `shouldSatisfy` String.contains (Pattern "i32.add")
      wat `shouldSatisfy` String.contains (Pattern "local.get $0")
      wat `shouldSatisfy` String.contains (Pattern "local.get $1")

    it "exports the function under its external name" do
      { wat } <- liftEffect buildAdd
      wat `shouldSatisfy` String.contains (Pattern "(export \"add\" (func $add))")

  describe "expressions" do
    it "builds a valid module returning an i32 constant" do
      ok <- liftEffect $ withModule \mod -> do
        body <- B.i32Const mod 42
        _ <- B.addFunction mod "answer" B.none B.i32 [] body
        B.validate mod
      ok `shouldEqual` true

  describe "integer ops, locals, blocks, and calls" do
    it "builds a valid module using i32Sub / i32Mul / localSet / block / call" do
      { ok } <- liftEffect buildOps
      ok `shouldEqual` true

    it "emits the expected instructions" do
      { wat } <- liftEffect buildOps
      wat `shouldSatisfy` String.contains (Pattern "i32.sub")
      wat `shouldSatisfy` String.contains (Pattern "i32.mul")
      -- the `localSet` into the declared var local, sequenced by a `block`
      wat `shouldSatisfy` String.contains (Pattern "local.set $2")
      -- the direct `call` to the other function
      wat `shouldSatisfy` String.contains (Pattern "call $helper")

  describe "Wasm GC (rec group, struct/array, cast)" do
    it "builds and validates a recursive type group with a box/ADT round-trip" do
      { ok } <- liftEffect buildGc
      ok `shouldEqual` true

    it "emits the expected GC instructions" do
      { wat } <- liftEffect buildGc
      wat `shouldSatisfy` String.contains (Pattern "struct.new")
      wat `shouldSatisfy` String.contains (Pattern "struct.get")
      wat `shouldSatisfy` String.contains (Pattern "array.new_fixed")
      wat `shouldSatisfy` String.contains (Pattern "array.get")
      wat `shouldSatisfy` String.contains (Pattern "ref.cast")

  describe "Wasm GC closures (signature type, ref.func, call_ref)" do
    it "builds and validates a closure invoked through call_ref" do
      { ok } <- liftEffect buildClosure
      ok `shouldEqual` true

    it "emits ref.func and call_ref" do
      { wat } <- liftEffect buildClosure
      wat `shouldSatisfy` String.contains (Pattern "ref.func")
      wat `shouldSatisfy` String.contains (Pattern "call_ref")

  describe "emission" do
    it "wraps emitText output in a (module ...) form" do
      wat <- liftEffect $ withModule B.emitText
      wat `shouldSatisfy` String.contains (Pattern "(module")

    it "emits a binary starting with the wasm magic bytes" do
      bytes <- liftEffect $ withModule \mod -> do
        buildAddInto mod
        B.emitBinary mod
      -- "\0asm" magic + version word = an 8-byte header at minimum
      byteLength bytes `shouldSatisfy` (_ >= 8)
      magicPrefix bytes `shouldEqual` [ 0x00, 0x61, 0x73, 0x6d ]

-- | Build the `add` module and return whether it validates plus its WAT.
buildAdd :: Effect { ok :: Boolean, wat :: String }
buildAdd = withModule \mod -> do
  buildAddInto mod
  ok <- B.validate mod
  wat <- B.emitText mod
  pure { ok, wat }

-- | Build a two-function module exercising the integer ops, local set, block,
-- | and call bindings: `helper(x) = x * x` and `caller(a, b) = helper (a - b)`,
-- | where `a - b` is stashed in a declared var local. Returns whether it
-- | validates plus its WAT.
buildOps :: Effect { ok :: Boolean, wat :: String }
buildOps = withModule \mod -> do
  -- helper(x) = x * x  (uses i32Mul; two distinct local.get nodes, not shared)
  hx0 <- B.localGet mod 0 B.i32
  hx1 <- B.localGet mod 0 B.i32
  helperBody <- B.i32Mul mod hx0 hx1
  _ <- B.addFunction mod "helper" (B.createType [ B.i32 ]) B.i32 [] helperBody

  -- caller(a, b) = helper (a - b)
  a <- B.localGet mod 0 B.i32
  b <- B.localGet mod 1 B.i32
  diff <- B.i32Sub mod a b
  setDiff <- B.localSet mod 2 diff -- local $2 = a - b
  arg <- B.localGet mod 2 B.i32
  called <- B.call mod "helper" [ arg ] B.i32
  callerBody <- B.block mod [ setDiff, called ] B.i32
  _ <- B.addFunction mod "caller" (B.createType [ B.i32, B.i32 ]) B.i32 [ B.i32 ] callerBody
  _ <- B.addFunctionExport mod "caller" "caller"

  ok <- B.validate mod
  wat <- B.emitText mod
  pure { ok, wat }

-- | Build the Slice-1 runtime rec group and a function that round-trips a value
-- | through it, exercising the whole GC FFI: a recursive type group
-- | (`$ADT` referencing `$Vals`), `struct.new`/`array.new_fixed` to build, and
-- | `struct.get`/`array.get`/`ref.cast` to read a field back out as `i32`.
-- |
-- |   $Vals = (array (mut eqref))
-- |   $Int  = (struct (field i32))
-- |   $ADT  = (struct (field i32) (field (ref $Vals)))
-- |
-- | The function builds `$ADT{ tag: 5, fields: [ box(7) ] }`, then reads field 0
-- | back to the `i32` 7.
buildGc :: Effect { ok :: Boolean, wat :: String }
buildGc = withModule \mod -> do
  B.setFeaturesGC mod
  tb <- B.typeBuilderCreate 3
  B.typeBuilderSetArrayType tb 0 B.eqref true -- $Vals
  B.typeBuilderSetStructType tb 1 [ { ty: B.i32, mutable: false } ] -- $Int
  valsTmp <- B.typeBuilderGetTempHeapType tb 0
  refVals <- B.typeBuilderGetTempRefType tb valsTmp false
  B.typeBuilderSetStructType tb 2 -- $ADT references $Vals (the recursive bit)
    [ { ty: B.i32, mutable: false }, { ty: refVals, mutable: false } ]
  hts <- B.typeBuilderBuildAndDispose tb 3
  case hts of
    [ valsHt, intHt, adtHt ] -> do
      seven <- B.i32Const mod 7
      boxed <- B.structNew mod intHt [ seven ]
      tag <- B.i32Const mod 5
      vals <- B.arrayNewFixed mod valsHt [ boxed ]
      adt <- B.structNew mod adtHt [ tag, vals ]
      fieldsArr <- B.structGet mod 1 adt (B.typeFromHeapType valsHt false) false
      idx0 <- B.i32Const mod 0
      elem <- B.arrayGet mod fieldsArr idx0 B.eqref false
      elemInt <- B.refCast mod elem (B.typeFromHeapType intHt false)
      val <- B.structGet mod 0 elemInt B.i32 false
      _ <- B.addFunction mod "gcRoundTrip" B.none B.i32 [] val
      _ <- B.addFunctionExport mod "gcRoundTrip" "gcRoundTrip"
      ok <- B.validate mod
      wat <- B.emitText mod
      pure { ok, wat }
    _ -> pure { ok: false, wat: "<expected 3 heap types>" }

-- | Build a closure and call it through `call_ref`, exercising the closure FFI:
-- | a function signature heap type (`$Code`), a closure struct holding a
-- | `funcref` + captured-env array (`$Clo`), `ref.func` to capture the code, and
-- | `call_ref` (after casting the stored `funcref` to `(ref $Code)`) to invoke
-- | it. The closure's code ignores its environment and returns its argument, so
-- | this stays focused on the call mechanism.
-- |
-- |   $Vals = (array (mut eqref)); $Int = (struct i32)
-- |   $Clo  = (struct funcref (ref $Vals))           -- main rec group
-- |   $Code = (func (ref $Clo) eqref -> eqref)        -- standalone (so addFunction's type matches)
buildClosure :: Effect { ok :: Boolean, wat :: String }
buildClosure = withModule \mod -> do
  B.setFeaturesGC mod
  tb1 <- B.typeBuilderCreate 3
  B.typeBuilderSetArrayType tb1 0 B.eqref true
  B.typeBuilderSetStructType tb1 1 [ { ty: B.i32, mutable: false } ]
  refValsTmp <- B.typeBuilderGetTempHeapType tb1 0 >>= \h -> B.typeBuilderGetTempRefType tb1 h false
  B.typeBuilderSetStructType tb1 2 [ { ty: B.funcref, mutable: false }, { ty: refValsTmp, mutable: false } ]
  g1 <- B.typeBuilderBuildAndDispose tb1 3
  case g1 of
    [ valsHt, intHt, cloHt ] -> do
      let refClo = B.typeFromHeapType cloHt false
      tb2 <- B.typeBuilderCreate 1
      B.typeBuilderSetSignatureType tb2 0 (B.createType [ refClo, B.eqref ]) B.eqref
      g2 <- B.typeBuilderBuildAndDispose tb2 1
      case g2 of
        [ codeHt ] -> do
          let refCode = B.typeFromHeapType codeHt false
          -- code(clo, y) = y
          y <- B.localGet mod 1 B.eqref
          _ <- B.addFunction mod "idCode" (B.createType [ refClo, B.eqref ]) B.eqref [] y
          -- run() = (closure idCode [box 0]).code(closure, box 5)
          dummy <- B.i32Const mod 0 >>= \z -> B.structNew mod intHt [ z ]
          envArr <- B.arrayNewFixed mod valsHt [ dummy ]
          fref <- B.refFunc mod "idCode" codeHt
          closure <- B.structNew mod cloHt [ fref, envArr ]
          setC <- B.localSet mod 0 closure
          codeF <- B.localGet mod 0 refClo
            >>= \c -> B.structGet mod 0 c B.funcref false
              >>= \f -> B.refCast mod f refCode
          cloArg <- B.localGet mod 0 refClo
          arg5 <- B.i32Const mod 5 >>= \f -> B.structNew mod intHt [ f ]
          callr <- B.callRef mod codeF [ cloArg, arg5 ] codeHt
          body <- B.block mod [ setC, callr ] B.eqref
          _ <- B.addFunction mod "run" B.none B.eqref [ refClo ] body
          _ <- B.addFunctionExport mod "run" "run"
          ok <- B.validate mod
          wat <- B.emitText mod
          pure { ok, wat }
        _ -> pure { ok: false, wat: "<expected 1 code heap type>" }
    _ -> pure { ok: false, wat: "<expected 3 heap types>" }

-- | Byte length of an emitted wasm binary.
foreign import byteLength :: Uint8Array -> Int

-- | The first four bytes of an emitted wasm binary, as ints.
foreign import magicPrefix :: Uint8Array -> Array Int

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec