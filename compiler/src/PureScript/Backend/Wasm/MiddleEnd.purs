-- | The middle-end (optimization layer) facade: run a program through the middle
-- | IR (ADR 0005) — translate each module's CoreFn to the MIR, apply the
-- | optimization passes, and translate back. Optimization is **whole-program**:
-- | dictionary elimination inlines across module boundaries, so the passes run over
-- | all linked modules together rather than one at a time.
-- |
-- | Pipeline: lambda lifting (per module) → dictionary elimination (whole-program
-- | simplification driven by a context built from every module). Back on CoreFn the
-- | rest of the lowering is unchanged.
module PureScript.Backend.Wasm.MiddleEnd
  ( optimizeProgram
  , optimizeModule
  ) where

import Prelude

import Data.Array as Array
import PureScript.Backend.Wasm.MiddleEnd.IR as M
import PureScript.Backend.Wasm.MiddleEnd.Optimize.DictElim as DictElim
import PureScript.Backend.Wasm.MiddleEnd.Optimize.LambdaLift (lambdaLiftModule)
import PureScript.Backend.Wasm.MiddleEnd.Transl (translBind)
import PureScript.Backend.Wasm.MiddleEnd.Untransl (untranslBind)
import PureScript.CoreFn (Module)

-- | Optimize a whole program. Each module's metadata (name, imports, foreign, …) is
-- | preserved; only its top-level bindings are rewritten — lambda lifting may also
-- | prepend lifted supercombinators.
optimizeProgram :: Array Module -> Array Module
optimizeProgram modules =
  Array.zipWith (\orig opt -> orig { decls = map untranslBind opt.decls }) modules optimized
  where
  mir = map (\m -> { name: m.name, decls: map translBind m.decls } :: M.Module) modules
  lifted = map lambdaLiftModule mir
  ctx = DictElim.buildCtx lifted
  optimized = map (DictElim.simplifyModule ctx) lifted

-- | Optimize a single self-contained module (its own bindings only). A convenience
-- | for callers with one module; cross-module dictionary elimination needs
-- | `optimizeProgram` over all linked modules.
optimizeModule :: Module -> Module
optimizeModule m = case optimizeProgram [ m ] of
  [ m' ] -> m'
  _ -> m
