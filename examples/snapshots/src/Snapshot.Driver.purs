module Snapshot.Driver where

import Prelude

import Data.Function.Uncurried (mkFn2, runFn2)
import Data.Tuple (Tuple(..))
import Snapshot.Cps02 (runState, test4)
import Snapshot.KnownConstructors06 (Test(..))

-- direct Fn2 round-trip: should be n + 10
fnTest :: Int -> Int
fnTest n = runFn2 (mkFn2 (\a b -> a + b)) n 10

-- runState n test4: test4 does get/put twice (state +1 +1), so result state = n + 2
cpsTest :: Int -> Int
cpsTest n = case runState n test4 of Tuple s _ -> s

-- genericShow of a nullary constructor
kcShow :: Int -> String
kcShow _ = show Baz
