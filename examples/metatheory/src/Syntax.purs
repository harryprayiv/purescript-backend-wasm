module Examples.Metatheory.Syntax where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Fmt as Fmt

import Examples.Metatheory.Primitive (Primitive)

newtype Var = Var String

derive newtype instance Eq Var
derive newtype instance Ord Var
instance Show Var where
  show (Var s) = Fmt.fmt @"(Var {s})" { s }

data Type_
  = TyInt
  | TyBool
  | TyVar Var
  | TyArr Type_ Type_ -- Function type
  | TyAbs Var Type_ -- Forall-quantified type is a type abstraction

derive instance Generic Type_ _
derive instance Eq Type_
instance Show Type_ where
  show it = genericShow it

data Constant
  = CstInt Int
  | CstBool Boolean

derive instance Generic Constant _
derive instance Eq Constant
instance Show Constant where
  show = genericShow

data Expr
  = ExprLit Constant
  | ExprVar Var
  | ExprPrim Primitive (Array Expr)
  | ExprAbs Var Type_ Expr
  | ExprApp Expr Expr
  | ExprTyApp Expr Type_
  | ExprIf Expr Expr Expr
  | ExprLet Var Expr Expr

-- | ExprLetrec Var Expr Expr

derive instance Generic Expr _
derive instance Eq Expr
instance Show Expr where
  show e = genericShow e