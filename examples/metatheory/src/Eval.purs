module Eval where

import Prelude

import Data.Array (findIndex, length, span, takeWhile, uncons)
import Data.Maybe (Maybe(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Examples.Metatheory.Primitive (Primitive(..))
import Examples.Metatheory.Syntax (Constant(..), Type_(..), Var)
import Examples.Metatheory.Typecheck (TypedExpr(..))

data EvalResult a
  = EvalStep TypedExpr
  | EvalDone a
  | EvalStuck

data NormalForm
  = NFInt Int
  | NFBool Boolean
  | NFAbs Var Type_ TypedExpr

isNormalForm :: TypedExpr -> Boolean
isNormalForm = case _ of
  TxprLit _ _ -> true
  TxprAbs _ _ _ _ -> true
  _ -> false

step :: TypedExpr -> EvalResult { val :: NormalForm, typ :: Type_ }
step = case _ of
  TxprLit typ (CstInt n) -> EvalDone { val: NFInt n, typ }
  TxprLit typ (CstBool b) -> EvalDone { val: NFBool b, typ }
  TxprAbs typ x t1 body -> EvalDone { val: NFAbs x t1 body, typ }
  TxprApp typ e1@(TxprAbs _ v _ body) e2 ->
    case step e2 of
      EvalStep e2' -> EvalStep (TxprApp typ e1 e2')
      EvalDone _ -> EvalStep (subst v e2 body)
      EvalStuck -> EvalStuck
  TxprPrim typ prim args -> do
    let { init: nfArgs, rest } = span isNormalForm args
    case uncons rest of
      Nothing -> evalPrim prim args
      Just { head: rdxArg, tail: rest' } -> do
        case step rdxArg of
          EvalStep rdxArg' -> EvalStep (TxprPrim typ prim (nfArgs <> [ rdxArg' ] <> rest'))
          EvalStuck -> EvalStuck
          EvalDone _ -> {- unreachable case -}  EvalStuck
  _ -> EvalStuck

  where
  evalPrim = case _, _ of
    PrimAdd, [ TxprLit _ (CstInt i1), (TxprLit _ (CstInt i2)) ] -> EvalStep $ TxprLit TyInt (CstInt (i1 + i2))
    PrimSub, [ TxprLit _ (CstInt i1), (TxprLit _ (CstInt i2)) ] -> EvalStep $ TxprLit TyInt (CstInt (i1 - i2))
    PrimMul, [ TxprLit _ (CstInt i1), (TxprLit _ (CstInt i2)) ] -> EvalStep $ TxprLit TyInt (CstInt (i1 * i2))
    PrimEqInt, [ TxprLit _ (CstInt i1), (TxprLit _ (CstInt i2)) ] -> EvalStep $ TxprLit TyBool (CstBool (i1 == i2))
    PrimCompInt, [ TxprLit _ (CstInt i1), (TxprLit _ (CstInt i2)) ] -> EvalStep $ TxprLit TyInt $ CstInt (compInt i1 i2)
    PrimIsZero, [ TxprLit _ (CstInt i) ] -> EvalStep $ TxprLit TyBool $ CstBool (i == 0)
    _, _ -> EvalStuck
    where
    compInt i1 i2 = case compare i1 i2 of
      EQ -> 0
      LT -> -1
      GT -> 1

  subst :: Var -> TypedExpr -> TypedExpr -> TypedExpr
  subst v e' e = case e of
    TxprLit _ _ -> e
    TxprVar _ x
      | x == v -> e'
      | otherwise -> e
    TxprAbs t v1 t1 body
      | v1 == v -> e -- if v is shadowed, we do not substite to shadowing variable.
      | otherwise -> TxprAbs t v1 t1 (subst v e' body)
    TxprApp t e1 e2 -> TxprApp t (subst v e' e1) (subst v e' e2)
    TxprLet t n t1 e1 e2
      | n == v -> TxprLet t n t1 (subst v e' e1) e2
      | otherwise -> TxprLet t n t1 (subst v e' e1) (subst v e' e2)
    TxprIf t e1 e2 e3 -> TxprIf t (subst v e' e1) (subst v e' e2) (subst v e' e3)
    TxprTyApp t e1 t1 -> TxprTyApp t (subst v e' e1) t1
    TxprPrim tprim prim args -> TxprPrim tprim prim ((subst v e') <$> args)