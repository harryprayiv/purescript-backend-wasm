module Snapshot.RecRepro (test, test2, test3) where

import Prelude

import Data.Maybe (Maybe(..))

-- record pattern nested inside a constructor pattern in a `case` — the path that
-- only `Lower.Match.compileRecord` handles (the `let { .. } =` path is separate).
test :: Maybe { x :: Int, y :: Int } -> Int
test m = case m of
  Just { x, y } -> x + y
  Nothing -> 0

-- two scrutinees, each a record-in-constructor (mirrors metatheory's zipMaybe)
test2 :: Maybe { a :: Int } -> Maybe { a :: Int } -> Int
test2 mx my = case mx, my of
  Just { a: ax }, Just { a: ay } -> ax + ay
  _, _ -> 0

-- Int -> Int driver (builds the Maybe internally) so we can call it across the JS
-- boundary without ADT-argument marshalling: confirms `compileRecord` is *correct*.
test3 :: Int -> Int
test3 n = test (Just { x: n, y: 10 }) + test2 (Just { a: n }) (Just { a: 100 })
