-- | Bridge from `purs`'s externs (`externs.cbor`) to the backend's representation
-- | choices. CoreFn is type-erased, but the externs retain each data
-- | constructor's *type* — so this is where top-level type information re-enters
-- | the pipeline (ADR 0013, front B): a constructor field that is concretely
-- | `Int`/`Char`/`Number` can be stored unboxed (`i32`/`f64`) in the constructor's
-- | struct instead of as a boxed `eqref`. The same externs are the foundation for
-- | later type-directed work (nominal record / dictionary layout).
module PureScript.Backend.Wasm.Externs
  ( ctorFieldReps
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import PureScript.Backend.Wasm.Lower.IR (Rep(..))
import PureScript.ExternsFile (ExternsDeclaration(..), ExternsFile(..))
import PureScript.ExternsFile.Names (ModuleName(..), ProperName(..), Qualified(..), QualifiedBy(..))
import PureScript.ExternsFile.Types as T

-- | Map every data constructor (by qualified name, `Module.Ctor`) to the wasm
-- | representation of each of its fields, in field order: `I32` for `Int`/`Char`,
-- | `F64` for `Number`, `Boxed` for anything else (polymorphic vars, other ADTs,
-- | records, …). Constructors absent from this table (no externs supplied) default
-- | to all-`Boxed` at the use site, so the table is purely an optimisation input.
ctorFieldReps :: Array ExternsFile -> Object (Array Rep)
ctorFieldReps externs = Object.fromFoldable (externs >>= declsOf)
  where
  declsOf (ExternsFile _ (ModuleName mn) _ _ _ _ decls _) = Array.mapMaybe (ctorOf mn) decls
  ctorOf mn = case _ of
    EDDataConstructor (ProperName ctorName) _ _ ty _ ->
      Just (Tuple (mn <> "." <> ctorName) (map scalarRep (fieldTypes ty)))
    _ -> Nothing

-- | The argument types of a curried function type, in order. A constructor's
-- | externs type is `field0 -> field1 -> … -> T`, encoded as nested `TypeApp`s of
-- | the `Prim.Function` constructor (`A -> B` is `(Function A) B`), so peeling that
-- | spine yields exactly the field types.
fieldTypes :: forall a. T.Type a -> Array (T.Type a)
fieldTypes = case _ of
  T.TypeApp _ (T.TypeApp _ (T.TypeConstructor _ fn) arg) rest
    | isFunction fn -> Array.cons arg (fieldTypes rest)
  _ -> []

isFunction :: Qualified ProperName -> Boolean
isFunction = case _ of
  Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Function") -> true
  _ -> false

-- | The representation a concretely-typed scalar field gets. Only the primitive
-- | scalar type constructors are recognised; everything else stays boxed.
scalarRep :: forall a. T.Type a -> Rep
scalarRep = case _ of
  T.TypeConstructor _ (Qualified _ (ProperName n))
    | n == "Int" || n == "Char" -> I32
    | n == "Number" -> F64
  _ -> Boxed
