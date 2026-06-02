-- | A CoreFn → CoreFn pre-pass: **lambda-lift self-recursive local functions** to
-- | top-level supercombinators. A `let`/`where`-bound function that recurses on
-- | itself (a single-binding `Rec` group whose value is a lambda — the `where go a
-- | b = … go a' b'` loop idiom) is moved out to a fresh top-level binding whose
-- | parameters are its captured free variables followed by its own parameters; the
-- | reference to it becomes that top-level name partially applied to the captures.
-- |
-- | Why: a local function's self-call goes through its closure (`call_ref` / an
-- | `RApply`), which the `return_call`-based tail-call elimination does not reach.
-- | After lifting, the (saturated) self-call is a direct call to a known top-level
-- | function (`RCallKnown`), so a tail self-call is eliminated like any other direct
-- | tail call — this is what makes `fib`'s `go` loop run in constant stack.
-- |
-- | Scope: only single-binding self-recursive `Rec` function bindings are lifted.
-- | Non-recursive local functions and mutually-recursive groups stay as closures
-- | (they are not the tail-recursion-loop case this targets).
module PureScript.Backend.Wasm.Lower.LambdaLift
  ( lambdaLiftModule
  ) where

import Prelude

import Control.Monad.State (State, gets, modify_, runState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Wasm.Lower.FreeVars (freeVars)
import PureScript.Backend.Wasm.Lower.Types (peelAbs)
import PureScript.CoreFn (Ann, Bind(..), Module, ModuleName, Qualified(..))
import PureScript.CoreFn as C

type Sub = Tuple String C.Expr

type LiftM = State { counter :: Int, lifted :: Array Bind }

-- | Lift every self-recursive local function in the module to a top-level
-- | supercombinator, prepending the new bindings to the module's declarations.
lambdaLiftModule :: Module -> Module
lambdaLiftModule m =
  case runState (traverse (liftBind m.name) m.decls) { counter: 0, lifted: [] } of
    Tuple decls st -> m { decls = st.lifted <> decls }

liftBind :: ModuleName -> Bind -> LiftM Bind
liftBind modName = case _ of
  NonRec ann ident e -> NonRec ann ident <$> liftExpr modName e
  Rec rs -> Rec <$> traverse (\r -> (\e -> r { expr = e }) <$> liftExpr modName r.expr) rs

liftExpr :: ModuleName -> C.Expr -> LiftM C.Expr
liftExpr modName = go
  where
  go = case _ of
    C.Literal ann lit -> C.Literal ann <$> goLit lit
    e@(C.Constructor _ _ _ _) -> pure e
    C.Accessor ann l e -> C.Accessor ann l <$> go e
    C.ObjectUpdate ann e cf kvs -> C.ObjectUpdate ann <$> go e <*> pure cf <*> traverse (traverse go) kvs
    C.Abs ann p b -> C.Abs ann p <$> go b
    C.App ann f a -> C.App ann <$> go f <*> go a
    e@(C.Var _ _) -> pure e
    C.Case ann ss alts -> C.Case ann <$> traverse go ss <*> traverse goAlt alts
    C.Let ann binds body -> liftLet modName ann binds body
  goLit = case _ of
    C.LitArray es -> C.LitArray <$> traverse go es
    C.LitObject kvs -> C.LitObject <$> traverse (traverse go) kvs
    other -> pure other
  goAlt alt = do
    result <- case alt.result of
      Right e -> Right <$> go e
      Left guards -> Left <$> traverse (\g -> { guard: _, expression: _ } <$> go g.guard <*> go g.expression) guards
    pure (alt { result = result })

-- | Process a `let`'s bindings left to right: lift each self-recursive function
-- | binding (recording the substitution to apply downstream) and keep the rest.
liftLet :: ModuleName -> Ann -> Array Bind -> C.Expr -> LiftM C.Expr
liftLet modName ann binds body = go [] [] binds
  where
  go kept subs bs = case Array.uncons bs of
    Nothing -> do
      body' <- liftExpr modName (applySubs subs body)
      pure case kept of
        [] -> body'
        _ -> C.Let ann kept body'
    Just { head, tail } -> case substBind subs head of
      Rec [ r ] | isAbs r.expr -> do
        sub <- liftSelfRecFn modName r.ident r.expr
        go kept (Array.snoc subs sub) tail
      other -> do
        other' <- liftBind modName other
        go (Array.snoc kept other') subs tail
  isAbs = case _ of
    C.Abs _ _ _ -> true
    _ -> false

-- | Lift one self-recursive function `ident = \params… -> body` to a fresh
-- | top-level `ident$liftN = \frees… params… -> body'`, returning the substitution
-- | `ident ↦ ident$liftN frees…` (the supercombinator partially applied to its
-- | captured free variables) for use at the reference sites.
liftSelfRecFn :: ModuleName -> String -> C.Expr -> LiftM Sub
liftSelfRecFn modName ident lambda = do
  let { params, body } = peelAbs lambda
  let frees = Array.filter (_ /= ident) (freeVars params body)
  n <- gets _.counter
  modify_ \s -> s { counter = s.counter + 1 }
  let
    liftedIdent = ident <> "$lift" <> show n
    liftedVar = C.Var synthAnn (Qualified (Just modName) liftedIdent)
    -- the replacement for `ident`: the supercombinator applied to the captures
    repl = foldl (\acc f -> C.App synthAnn acc (localVar f)) liftedVar frees
  -- inside the lifted body, the self reference becomes the same partial application
  -- (the captures resolve to the leading parameters there), then lift nested locals
  body' <- liftExpr modName (substVar ident repl body)
  let lambda' = foldr (C.Abs synthAnn) body' (frees <> params)
  modify_ \s -> s { lifted = Array.snoc s.lifted (NonRec synthAnn liftedIdent lambda') }
  pure (Tuple ident repl)

-- substitution ---------------------------------------------------------------

applySubs :: Array Sub -> C.Expr -> C.Expr
applySubs subs e = foldl (\acc (Tuple n r) -> substVar n r acc) e subs

substBind :: Array Sub -> Bind -> Bind
substBind subs = case _ of
  NonRec a i e -> NonRec a i (applySubs subs e)
  Rec rs -> Rec (map (\r -> r { expr = applySubs subs r.expr }) rs)

-- | Replace free occurrences of the local `name` with `repl`, stopping at any
-- | binder that rebinds `name` (capture avoidance). `repl` only introduces
-- | references to already-in-scope names, so no further freshening is needed.
substVar :: String -> C.Expr -> C.Expr -> C.Expr
substVar name repl = go
  where
  go = case _ of
    e@(C.Var _ (Qualified Nothing n)) -> if n == name then repl else e
    e@(C.Var _ _) -> e
    C.Literal ann lit -> C.Literal ann (goLit lit)
    e@(C.Constructor _ _ _ _) -> e
    C.Accessor ann l e -> C.Accessor ann l (go e)
    C.ObjectUpdate ann e cf kvs -> C.ObjectUpdate ann (go e) cf (map (map go) kvs)
    C.Abs ann p b -> if p == name then C.Abs ann p b else C.Abs ann p (go b)
    C.App ann f a -> C.App ann (go f) (go a)
    C.Case ann ss alts -> C.Case ann (map go ss) (map goAlt alts)
    C.Let ann binds body ->
      if Array.elem name (binds >>= boundNames) then C.Let ann binds body
      else C.Let ann (map goBind binds) (go body)
  goLit = case _ of
    C.LitArray es -> C.LitArray (map go es)
    C.LitObject kvs -> C.LitObject (map (map go) kvs)
    other -> other
  goAlt alt =
    if Array.elem name (alt.binders >>= binderNames) then alt
    else alt { result = goResult alt.result }
  goResult = case _ of
    Right e -> Right (go e)
    Left gs -> Left (map (\g -> { guard: go g.guard, expression: go g.expression }) gs)
  goBind = case _ of
    NonRec a i e -> NonRec a i (go e)
    Rec rs -> Rec (map (\r -> r { expr = go r.expr }) rs)

boundNames :: Bind -> Array String
boundNames = case _ of
  NonRec _ i _ -> [ i ]
  Rec rs -> map _.ident rs

binderNames :: C.Binder -> Array String
binderNames = case _ of
  C.NullBinder _ -> []
  C.LiteralBinder _ lit -> case lit of
    C.LitArray bs -> bs >>= binderNames
    C.LitObject fields -> fields >>= \(Tuple _ b) -> binderNames b
    _ -> []
  C.VarBinder _ n -> [ n ]
  C.ConstructorBinder _ _ _ bs -> bs >>= binderNames
  C.NamedBinder _ n b -> Array.cons n (binderNames b)

localVar :: String -> C.Expr
localVar n = C.Var synthAnn (Qualified Nothing n)

-- | A zero source annotation for synthesised CoreFn nodes.
synthAnn :: Ann
synthAnn = { meta: Nothing, span: { start: origin, end: origin } }
  where
  origin = { line: 0, column: 0 }
