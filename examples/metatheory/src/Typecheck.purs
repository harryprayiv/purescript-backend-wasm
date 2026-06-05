module Examples.Metatheory.Typecheck where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.State (class MonadState, State, StateT, evalStateT, get, modify_, put)
import Control.Monad.State.Class (class MonadState)
import Data.Array (foldl, uncons)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Examples.Metatheory.Primitive (Primitive(..))
import Examples.Metatheory.Syntax (Constant(..), Expr(..), Type_(..), Var(..))
import Fmt as Fmt

data TypedExpr
  = TxprLit Type_ Constant
  | TxprVar Type_ Var
  | TxprPrim Type_ Primitive (Array TypedExpr)
  | TxprAbs Type_ Var Type_ TypedExpr
  | TxprApp Type_ TypedExpr TypedExpr
  | TxprTyApp Type_ TypedExpr Type_
  | TxprIf Type_ TypedExpr TypedExpr TypedExpr
  | TxprLet Type_ Var Type_ TypedExpr TypedExpr

-- | TxprLetrec Type_ Var Type_ TypedExpr TypedExpr

derive instance Generic TypedExpr _
derive instance Eq TypedExpr
instance Show TypedExpr where
  show te = genericShow te

data TypeError
  = WShadowing Var
  | ETypeMismatch Type_ Type_
  | ENotAFunction Type_
  | ENotAType Type_
  | EUnboundVariable Var
  | EUnboundTypeVariable Var
  | EUnexpectedForall
  | EPrimArityMismatch
  | EOtherError String

derive instance Generic TypeError _
instance Show TypeError where
  show te = genericShow te

newtype TypeEnv = TypeEnv (M.Map Var Type_)

emptyEnv :: TypeEnv
emptyEnv = TypeEnv M.empty

lookupEnv :: Var -> TypeEnv -> Maybe Type_
lookupEnv v (TypeEnv env) = M.lookup v env

extend :: Var -> Type_ -> TypeEnv -> TypeEnv
extend v t (TypeEnv env) = TypeEnv (M.insert v t env)

type TypingState =
  { nextMeta :: Int
  , warnings :: Array TypeError
  }

newtype TypingM a = TypingM (ReaderT TypeEnv (StateT TypingState (Except TypeError)) a)

derive newtype instance Functor TypingM
derive newtype instance Apply TypingM
derive newtype instance Applicative TypingM
derive newtype instance Bind TypingM
derive newtype instance Monad TypingM
derive newtype instance MonadAsk TypeEnv TypingM
derive newtype instance MonadReader TypeEnv TypingM
derive newtype instance MonadState TypingState TypingM
derive newtype instance MonadThrow TypeError TypingM

runTyping :: TypeEnv -> TypingState -> TypingM ~> Either TypeError
runTyping env s (TypingM m) = runExcept (evalStateT (runReaderT m env) s)

freshMeta :: TypingM Var
freshMeta = do
  { nextMeta: m } <- get
  modify_ (_ { nextMeta = m + 1 })
  pure (Var (Fmt.fmt @"?m{m}" { m }))

failwith :: forall a. TypeError -> TypingM a
failwith = throwError

warn :: TypeError -> TypingM Unit
warn w = modify_ \s -> s { warnings = s.warnings <> [ w ] }

lookup :: Var -> TypingM (Maybe Type_)
lookup v = ask <#> lookupEnv v

typeOf :: TypedExpr -> Type_
typeOf = case _ of
  TxprLit t _ -> t
  TxprVar t _ -> t
  TxprPrim t _ _ -> t
  TxprAbs t _ _ _ -> t
  TxprApp t _ _ -> t
  TxprTyApp t _ _ -> t
  TxprIf t _ _ _ -> t
  TxprLet t _ _ _ _ -> t

-- TxprLetrec t _ _ _ _ -> t

typEq :: TypedExpr -> TypedExpr -> Boolean
typEq = (==) `on` typeOf

typing :: Expr -> TypingM TypedExpr
typing = case _ of
  ExprLit (CstInt n) -> pure (TxprLit TyInt (CstInt n))
  ExprLit (CstBool b) -> pure (TxprLit TyBool (CstBool b))

  ExprVar v -> lookup v >>= case _ of
    Just t -> pure (TxprVar t v)
    Nothing -> failwith (EUnboundVariable v)

  ExprAbs v t1 e -> do
    whenM (isJust <$> (lookup v)) do
      warn (WShadowing v)
    te <- withExtends [ v /\ t1 ] (typing e)
    pure (TxprAbs (TyArr t1 (typeOf te)) v t1 te)

  ExprApp e1 e2 -> do
    te1 <- typing e1
    te2 <- typing e2
    case typeOf te1 of
      TyArr tArg tRes
        | tArg == typeOf te2 -> pure (TxprApp tRes te1 te2)
        | otherwise -> failwith (ETypeMismatch tArg (typeOf te2))
      _ -> failwith (ENotAFunction (typeOf te1))

  ExprIf cond eThen eElse -> do
    teCond <- typing cond
    teThen <- typing eThen
    teElse <- typing eElse
    let
      t1 = typeOf teCond
      t2 = typeOf teThen
      t3 = typeOf teElse
    case t1 of
      TyBool -> do
        when (t2 /= t3) do
          failwith (ETypeMismatch t2 t3)
        pure (TxprIf t2 teCond teThen teElse)
      _ -> failwith (ETypeMismatch TyBool t1)

  ExprLet v e1 e2 -> do
    te1 <- typing e1
    whenM (isJust <$> (lookup v)) do
      warn (WShadowing v)
    te2 <- withExtends [ v /\ typeOf te1 ] (typing e2)
    pure (TxprLet (typeOf te2) v (typeOf te1) te1 te2)

  ExprPrim prim args ->
    let
      { typ: tPrim, args: tArgs } = typeofPrim prim
    in
      case zipMaybe tArgs args of
        Nothing -> failwith EPrimArityMismatch
        Just ts -> do
          typedArgs <- for ts (uncurry match)
          pure (TxprPrim tPrim prim typedArgs)
        _ -> failwith (EOtherError "Not implemented")

  _ -> failwith (EOtherError "Not implemented")

  where
  withExtends :: Array (Var /\ Type_) -> TypingM ~> TypingM
  withExtends bindings = local (extendAll bindings)

  extendAll :: Array (Var /\ Type_) -> TypeEnv -> TypeEnv
  extendAll bindings env = foldl (\e (v /\ t) -> extend v t e) env bindings

  match :: Type_ -> Expr -> _ TypedExpr
  match tExpect exp = do
    txp <- typing exp
    if typeOf txp == tExpect then pure txp
    else failwith (ETypeMismatch tExpect (typeOf txp))

  zipMaybe :: forall a b. Array a -> Array b -> Maybe (Array (a /\ b))
  zipMaybe = go []
    where
    go acc xs ys = case uncons xs, uncons ys of
      Just { head: x, tail: xs' }, Just { head: y, tail: ys' } -> go (Array.cons (x /\ y) acc) xs' ys'
      _, _ -> Nothing

typeofPrim :: Primitive -> { typ :: Type_, args :: Array Type_ }
typeofPrim = case _ of
  PrimAdd -> { typ: TyInt, args: [ TyInt, TyInt ] }
  PrimSub -> { typ: TyInt, args: [ TyInt, TyInt ] }
  PrimMul -> { typ: TyInt, args: [ TyInt, TyInt ] }
  PrimIsZero -> { typ: TyBool, args: [ TyInt ] }
  PrimEqInt -> { typ: TyBool, args: [ TyInt, TyInt ] }
  PrimCompInt -> { typ: TyInt, args: [ TyInt, TyInt ] }

typecheck :: Expr -> Either TypeError TypedExpr
typecheck e = typing e # runTyping emptyEnv { nextMeta: 0, warnings: [] }