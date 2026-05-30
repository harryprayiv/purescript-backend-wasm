-- | Lower the CoreFn AST to the backend IR (`PureScript.Backend.Wasm.IR`).
-- |
-- | This is the **Slice 0** lowering: the scalar `Int`-only world. It handles
-- | top-level function definitions whose bodies are built from integer
-- | literals, local variables, saturated applications of module-local foreign
-- | primitives (mapped to `Intrinsic`s), and saturated calls to other
-- | top-level functions. Anything outside that subset is reported as a
-- | `LowerError` rather than mis-compiled — Slice 0 would rather refuse than
-- | guess.
-- |
-- | The two transforms that matter here, and why:
-- |
-- |   * **Spine collection.** CoreFn curries application, so `f a b` is
-- |     `App (App f a) b`. We flatten the spine to a head plus an argument list
-- |     to decide saturation against a known arity (ADR 0003 eval/apply).
-- |
-- |   * **A-normalization.** Every operand must become an `Atom`. A nested
-- |     non-trivial argument (e.g. the inner call in `double (double x)`) is
-- |     hoisted into its own `Let`, which is exactly what makes evaluation order
-- |     explicit and gives each intermediate a wasm local.
module PureScript.Backend.Wasm.FromCoreFn
  ( lowerModule
  , LowerError(..)
  ) where

import Prelude

import Control.Monad.State (StateT, get, modify_, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import PureScript.CoreFn (Bind(..), Expr(Abs, App, Literal, Var), Literal(LitInt), Module, Qualified(..))
import PureScript.Backend.Wasm.IR (Atom(..), Block(..), FuncName(..), IRFunc, Intrinsic(..), Program, Rep(..), Rhs(..), Slot(..), VarRef(..))

-- | Why the lowering can fail at all: Slice 0 supports a strict subset, and a
-- | construct it does not yet handle (a partial application, a non-`Int`
-- | literal, a top-level `Rec` group, …) must surface explicitly so the gap is
-- | visible instead of silently producing wrong wasm.
data LowerError
  = UnsupportedExpr String
  | UnsupportedTopLevel String
  | UnknownVariable String
  | NotSaturated String Int Int -- name, expected arity, actual args

derive instance eqLowerError :: Eq LowerError
derive instance genericLowerError :: Generic LowerError _
instance showLowerError :: Show LowerError where
  show = genericShow

-- | A pending IR binding accumulated during lowering of one function body.
type Binding = { slot :: Slot, rep :: Rep, rhs :: Rhs }

-- | Lowering state for a single function: the next free local slot (parameters
-- | occupy the slots below `next`) and the bindings emitted so far, kept in
-- | evaluation order.
type LState = { next :: Int, binds :: Array Binding }

-- | `StateT` over `Either` so a failure short-circuits the whole module. The
-- | `Object Slot` reader (the local environment) is threaded explicitly as a
-- | function argument rather than via `ReaderT`, since it changes only when we
-- | descend under binders, which Slice 0 never does inside a body.
type Lower a = StateT LState (Either LowerError) a

throw :: forall a. LowerError -> Lower a
throw = lift <<< Left

-- | Allocate a fresh local slot.
fresh :: Lower Slot
fresh = do
  s <- get
  put s { next = s.next + 1 }
  pure (Slot s.next)

-- | Bind an `Rhs` to a fresh slot (in evaluation order) and return the atom
-- | that names its result. This is the single primitive that turns a
-- | computation into an A-normal-form `Atom`.
emit :: Rep -> Rhs -> Lower Atom
emit rep rhs = do
  slot <- fresh
  modify_ \s -> s { binds = Array.snoc s.binds { slot, rep, rhs } }
  pure (AVar (Local slot))

-- | The Slice 0 foreign-primitive table (ADR 0002's `ForeignProvider`, hard
-- | coded for now). Maps a module-local foreign identifier to the machine op it
-- | denotes together with that op's arity, used for the saturation check.
foreignIntrinsic :: String -> Maybe (Tuple Intrinsic Int)
foreignIntrinsic = case _ of
  "addI" -> Just (Tuple IntAdd 2)
  "mulI" -> Just (Tuple IntMul 2)
  "subI" -> Just (Tuple IntSub 2)
  _ -> Nothing

-- | Flatten a curried application spine into its head and left-to-right
-- | argument list: `App (App f a) b` becomes `{ head: f, args: [a, b] }`.
collectApp :: Expr -> { head :: Expr, args :: Array Expr }
collectApp = go []
  where
  go acc = case _ of
    App _ f a -> go (Array.cons a acc) f
    other -> { head: other, args: acc }

-- | Reduce an expression to a trivial `Atom`, hoisting any computation into
-- | `Let` bindings via `emit`.
lowerAtom :: Env -> Expr -> Lower Atom
lowerAtom env = case _ of
  Literal _ (LitInt n) -> pure (ALitInt n)
  Literal _ _ -> throw (UnsupportedExpr "non-Int literal")
  -- A local variable: resolve its CoreFn ident to a wasm slot.
  Var _ (Qualified Nothing ident) ->
    case Object.lookup ident env.locals of
      Just slot -> pure (AVar (Local slot))
      Nothing -> throw (UnknownVariable ident)
  -- A bare qualified name with no arguments is a value/partial reference, which
  -- Slice 0 does not model yet (top-level values arrive in Slice 1, partial
  -- application in Slice 2).
  Var _ (Qualified (Just _) ident) ->
    throw (UnsupportedExpr ("unapplied top-level/foreign reference: " <> ident))
  expr@(App _ _ _) -> do
    rhs <- lowerApp env (collectApp expr)
    emit I32 rhs
  Abs _ _ _ -> throw (UnsupportedExpr "nested lambda (Slice 2)")
  _ -> throw (UnsupportedExpr "unsupported expression in body")

-- | Lower an application spine to the `Rhs` that performs the call, classifying
-- | the head as either a foreign intrinsic or a known top-level function and
-- | checking that the call is saturated (ADR 0003).
lowerApp :: Env -> { head :: Expr, args :: Array Expr } -> Lower Rhs
lowerApp env { head, args } = case head of
  Var _ (Qualified (Just _) ident)
    | Just (Tuple intr arity) <- foreignIntrinsic ident ->
        if Array.length args == arity then RPrim intr <$> traverse (lowerAtom env) args
        else throw (NotSaturated ident arity (Array.length args))
    | Just arity <- Object.lookup ident env.knownFuncs ->
        if Array.length args == arity then RCallKnown (funcName env.moduleName ident) <$> traverse (lowerAtom env) args
        else throw (NotSaturated ident arity (Array.length args))
    | otherwise -> throw (UnsupportedExpr ("unknown callee: " <> ident))
  _ -> throw (UnsupportedExpr "application of a non-name head (Slice 2)")

-- | The local environment plus the read-only module facts the lowering needs.
type Env =
  { locals :: Object Slot -- in-scope local idents → slots
  , knownFuncs :: Object Int -- top-level function idents → arity
  , moduleName :: Array String
  }

-- | Qualify a top-level identifier into a globally-unique wasm function name.
funcName :: Array String -> String -> FuncName
funcName moduleName ident = FuncName (joinWith "." moduleName <> "." <> ident)

-- | Peel leading lambdas, returning the parameter idents (outermost first) and
-- | the function body.
peelAbs :: Expr -> { params :: Array String, body :: Expr }
peelAbs = go []
  where
  go acc = case _ of
    Abs _ p b -> go (Array.snoc acc p) b
    body -> { params: acc, body }

-- | Lower one top-level `NonRec` binding to an `IRFunc`. A binding with no
-- | leading lambdas (e.g. `five = addI 2 3`) becomes a nullary function; Slice 0
-- | does not yet give such CAFs their compute-once semantics (Slice 1), which is
-- | sound here because the values are pure `Int`s.
lowerFunc :: Object Int -> Array String -> String -> Expr -> Either LowerError IRFunc
lowerFunc knownFuncs moduleName ident expr = do
  let { params, body } = peelAbs expr
  let locals = Object.fromFoldable (Array.mapWithIndex (\i p -> Tuple p (Slot i)) params)
  let env = { locals, knownFuncs, moduleName }
  Tuple resultAtom st <- runStateT (lowerAtom env body) { next: Array.length params, binds: [] }
  let block = Array.foldr (\b acc -> Let b.slot b.rep b.rhs acc) (Ret resultAtom) st.binds
  pure
    { name: funcName moduleName ident
    , params: const I32 <$> params
    , result: I32
    , body: block
    , export: Just ident
    }

-- | Lower a whole decoded CoreFn module to a Slice 0 IR `Program`.
lowerModule :: Module -> Either LowerError Program
lowerModule m = do
  let knownFuncs = Object.fromFoldable (Array.mapMaybe topLevelArity m.decls)
  funcs <- traverse (lowerBind knownFuncs) (Array.mapMaybe asNonRec m.decls)
  pure { funcs }
  where
  lowerBind knownFuncs (Tuple ident expr) = lowerFunc knownFuncs m.name ident expr

  asNonRec = case _ of
    NonRec _ ident expr -> Just (Tuple ident expr)
    Rec _ -> Nothing

  topLevelArity = case _ of
    NonRec _ ident expr -> Just (Tuple ident (Array.length (peelAbs expr).params))
    Rec _ -> Nothing
