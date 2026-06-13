-- | The ADTs + pattern-matching E2E fixture (built standalone by
-- | `purs-wasm build -e E2E.DataTypes`, asserted by `Test.E2E.Cli.DataTypes`).
-- |
-- | Pure ADT construction and matching: a nullary constructor (`None`), a field
-- | constructor (`Some`), and a multi-field one (`Triple`); single-scrutinee,
-- | unguarded `case`; `Var`/wildcard binders. The exposed functions are
-- | `Int`-typed at the boundary so the host can drive them, while building and
-- | matching the ADTs internally. No foreign imports.
module E2E.DataTypes where

data OptInt = None | Some Int

data Triple = Triple Int Int Int

orElse :: OptInt -> Int -> Int
orElse o d = case o of
  None -> d
  Some x -> x

someOrElse :: Int -> Int
someOrElse n = orElse (Some n) 0

noneOrElse :: Int -> Int
noneOrElse d = orElse None d

third :: Int -> Int -> Int -> Int
third a b c = case Triple a b c of
  Triple _ _ z -> z
