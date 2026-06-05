module Examples.Metatheory.Primitive where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Primitive
  = PrimIsZero
  | PrimAdd
  | PrimMul
  | PrimSub
  | PrimEqInt
  | PrimCompInt

derive instance Generic Primitive _
derive instance Eq Primitive
instance Show Primitive where
  show = genericShow
