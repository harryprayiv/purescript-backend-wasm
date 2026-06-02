-- | Dictionary elimination (ADR 0005): the whole-program policy that drives the
-- | MIR `Simplify` engine. It scans every module to find the type-class plumbing —
-- | the transparent dictionary constructors, the method accessors that destructure
-- | them, and the instance dictionaries that construct them — and feeds them to the
-- | simplifier as its inline set and transparent-constructor set. The simplifier
-- | then collapses `accessor(instance)` down to the underlying implementation.
-- |
-- | This is necessarily whole-program: a use site like `Data.Eq.eq(eqInt)` lives in
-- | one module while `eq` and `eqInt` are defined in another, so the inline set is
-- | built across all linked modules.
module PureScript.Backend.Wasm.MiddleEnd.Optimize.DictElim
  ( buildCtx
  , simplifyModule
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..), snd)
import PureScript.Backend.Wasm.MiddleEnd.IR as M
import PureScript.Backend.Wasm.MiddleEnd.Optimize.Simplify (Ctx, simplifyExpr)
import PureScript.CoreFn (Binder(..), Literal(..), Meta(..), ModuleName, Qualified(..))

-- | Build the simplifier context for a whole program: the transparent dictionary
-- | constructors and the inlinable dictionary bindings, across all modules.
buildCtx :: Array M.Module -> Ctx
buildCtx modules =
  { newtypeCtors: ctors
  , dataCtors: Set.fromFoldable (modules >>= \m -> Array.mapMaybe (dataCtorName m.name) m.decls)
  , inline: Map.fromFoldable (modules >>= \m -> Array.mapMaybe (inlinable ctors m.name) m.decls)
  }
  where
  ctors = Set.fromFoldable (modules >>= \m -> Array.mapMaybe (dictCtorName m.name) m.decls)

simplifyModule :: Ctx -> M.Module -> M.Module
simplifyModule ctx m = m { decls = map go m.decls }
  where
  go = case _ of
    M.NonRec meta i e -> M.NonRec meta i (simplifyExpr ctx e)
    M.Rec rs -> M.Rec (map (\r -> r { expr = simplifyExpr ctx r.expr }) rs)

-- transparent (dictionary) constructors ---------------------------------------

-- | The dictionary constructors are the top-level identity bindings the compiler
-- | tags `IsTypeClassConstructor` (newtypes whose payload is the method record).
dictCtorName :: ModuleName -> M.Bind -> Maybe String
dictCtorName modName = case _ of
  M.NonRec (Just IsTypeClassConstructor) ident _ -> Just (key modName ident)
  _ -> Nothing

-- | Rigid data constructors are the top-level `Constructor` declarations; their
-- | name is matched by `case` in the simplifier.
dataCtorName :: ModuleName -> M.Bind -> Maybe String
dataCtorName modName = case _ of
  M.NonRec _ ident (M.Constructor _ _ _) -> Just (key modName ident)
  _ -> Nothing

-- inlinable dictionary bindings -----------------------------------------------

-- | A non-recursive binding is inlinable for dictionary elimination when it is a
-- | dictionary constructor, a method accessor (destructures a dictionary), or an
-- | instance (constructs one). Self-referential bindings (recursive instances) are
-- | excluded to keep inlining terminating.
inlinable :: Set String -> ModuleName -> M.Bind -> Maybe (Tuple String M.Expr)
inlinable ctors modName = case _ of
  M.NonRec meta ident rhs ->
    let
      k = key modName ident
    in
      if not (selfReferential k rhs) && (isDictCtor meta || isDictShaped ctors (bodyOf rhs)) then
        Just (Tuple k rhs)
      else
        Nothing
  M.Rec _ -> Nothing

isDictCtor :: Maybe Meta -> Boolean
isDictCtor = case _ of
  Just IsTypeClassConstructor -> true
  _ -> false

-- | After peeling any leading lambdas (parameterised instances), a dictionary
-- | binding either destructures a dictionary (a method accessor: a single-alt case
-- | on a transparent constructor) or constructs one (an instance: a dictionary
-- | constructor applied to its record).
isDictShaped :: Set String -> M.Expr -> Boolean
isDictShaped ctors = case _ of
  M.Case [ _ ] [ alt ] -> case alt.binders of
    [ ConstructorBinder _ _ ctor _ ] -> ctorMember ctors ctor
    _ -> false
  M.App (M.Var ctor) _ -> ctorMember ctors ctor
  _ -> false

bodyOf :: M.Expr -> M.Expr
bodyOf = case _ of
  M.Abs _ b -> bodyOf b
  e -> e

selfReferential :: String -> M.Expr -> Boolean
selfReferential k rhs = Array.elem k (qualKeys rhs)

-- | Every top-level (qualified) name an expression references.
qualKeys :: M.Expr -> Array String
qualKeys = case _ of
  M.Var q -> maybe [] pure (qkey q)
  M.Lit lit -> litExprs lit >>= qualKeys
  M.Constructor _ _ _ -> []
  M.Accessor _ e -> qualKeys e
  M.Update e _ kvs -> qualKeys e <> (kvs >>= qualKeys <<< snd)
  M.Abs _ b -> qualKeys b
  M.App f args -> qualKeys f <> (args >>= qualKeys)
  M.Case scruts alts -> (scruts >>= qualKeys) <> (alts >>= altExprs >>= qualKeys)
  M.Let binds body -> (binds >>= bindExprs >>= qualKeys) <> qualKeys body
  where
  litExprs = case _ of
    LitArray es -> es
    LitObject kvs -> map snd kvs
    _ -> []
  altExprs alt = case alt.result of
    Right e -> [ e ]
    Left gs -> gs >>= \g -> [ g.guard, g.expression ]
  bindExprs = case _ of
    M.NonRec _ _ e -> [ e ]
    M.Rec rs -> map _.expr rs

-- keys ------------------------------------------------------------------------

ctorMember :: Set String -> Qualified String -> Boolean
ctorMember ctors q = maybe false (_ `Set.member` ctors) (qkey q)

key :: ModuleName -> String -> String
key modName ident = joinWith "." modName <> "." <> ident

qkey :: Qualified String -> Maybe String
qkey = case _ of
  Qualified (Just m) n -> Just (joinWith "." m <> "." <> n)
  Qualified Nothing _ -> Nothing
