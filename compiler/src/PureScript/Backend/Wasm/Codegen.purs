-- | Lower the backend IR (`PureScript.Backend.Wasm.IR`) to a Binaryen module.
-- |
-- | This is the **Slice 0** code generator: the IR it consumes is the scalar
-- | `Int`-only subset, so every value is an `i32`. The mapping is deliberately
-- | mechanical — all the hard decisions were made during lowering (ADR 0003):
-- |
-- |   * an IR `Slot` is a wasm local *index* directly (parameters first, then
-- |     `Let`-bound temporaries, in slot order);
-- |   * a `Let` becomes a `local.set` statement, and the chain of statements
-- |     plus the tail value are sequenced inside a `block` whose value is the
-- |     last child;
-- |   * `RPrim` becomes an inline `i32` op, `RCallKnown` a direct `call`.
-- |
-- | The generator assumes the IR satisfies the invariants the lowering
-- | guarantees (e.g. a binary intrinsic has exactly two operands); a violation
-- | raises an `Effect` exception rather than producing malformed wasm.
module PureScript.Backend.Wasm.Codegen
  ( buildModule
  ) where

import Prelude

import Binaryen as B
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (error, throwException)
import PureScript.Backend.Wasm.IR (Atom(..), Block(..), FuncName(..), IRFunc, Intrinsic(..), Program, Rep(..), Rhs(..), Slot(..), VarRef(..))

-- | Build a Binaryen module from a Slice 0 IR `Program`. Functions are added
-- | first, then exports; `RCallKnown` references functions by name, which
-- | Binaryen resolves at validation time, so definition order does not matter.
buildModule :: Program -> Effect B.Module
buildModule prog = do
  mod <- B.createModule
  traverse_ (addFunc mod) prog.funcs
  traverse_ (addExport mod) prog.funcs
  pure mod

-- | The wasm type for a chosen representation. Slice 0 only ever uses `I32`;
-- | `Boxed` maps to the universal `eqref` box once Slice 1 defines it, and is
-- | unreachable here.
repType :: Rep -> B.Type
repType = case _ of
  I32 -> B.i32
  F64 -> B.f64
  Boxed -> B.i32 -- placeholder; unreachable in Slice 0 (see ADR 0001 for the eqref box)

funcNameStr :: FuncName -> String
funcNameStr (FuncName n) = n

addFunc :: B.Module -> IRFunc -> Effect Unit
addFunc mod fn = do
  body <- genBody mod fn.body
  let params = B.createType (repType <$> fn.params)
  -- Locals beyond the parameters are exactly the `Let`-bound slots, in slot
  -- order — which is the order they appear walking the block top-down.
  let varTypes = repType <$> letReps fn.body
  _ <- B.addFunction mod (funcNameStr fn.name) params (repType fn.result) varTypes body
  pure unit

addExport :: B.Module -> IRFunc -> Effect Unit
addExport mod fn = case fn.export of
  Just external -> void (B.addFunctionExport mod (funcNameStr fn.name) external)
  Nothing -> pure unit

-- | The representations of a block's `Let` bindings, in order.
letReps :: Block -> Array Rep
letReps = case _ of
  Ret _ -> []
  Let _ rep _ k -> Array.cons rep (letReps k)

-- | Generate the function body: each `Let` is a `local.set` statement, and the
-- | statements plus the tail value are wrapped in a `block`. A body that is a
-- | bare `Ret` (no bindings) is emitted as the value expression directly.
genBody :: B.Module -> Block -> Effect B.Expression
genBody mod = go []
  where
  go statements = case _ of
    Ret atom -> do
      value <- genAtom mod atom
      if Array.null statements then pure value
      else B.block mod (Array.snoc statements value) B.i32
    Let (Slot index) _rep rhs k -> do
      e <- genRhs mod rhs
      stmt <- B.localSet mod index e
      go (Array.snoc statements stmt) k

genAtom :: B.Module -> Atom -> Effect B.Expression
genAtom mod = case _ of
  ALitInt n -> B.i32Const mod n
  AVar (Local (Slot index)) -> B.localGet mod index B.i32

genRhs :: B.Module -> Rhs -> Effect B.Expression
genRhs mod = case _ of
  RAtom atom -> genAtom mod atom
  RPrim intr args -> genPrim mod intr args
  RCallKnown name args -> do
    operands <- traverse (genAtom mod) args
    B.call mod (funcNameStr name) operands B.i32

-- | Slice 0 intrinsics are all binary `i32` ops; the lowering guarantees the
-- | arity, so a different operand count is an internal error.
genPrim :: B.Module -> Intrinsic -> Array Atom -> Effect B.Expression
genPrim mod intr = case _ of
  [ a, b ] -> do
    ea <- genAtom mod a
    eb <- genAtom mod b
    case intr of
      IntAdd -> B.i32Add mod ea eb
      IntSub -> B.i32Sub mod ea eb
      IntMul -> B.i32Mul mod ea eb
  _ -> throwException (error "Codegen: binary intrinsic given a non-binary operand list")
