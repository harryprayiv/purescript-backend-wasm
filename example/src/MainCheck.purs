-- | Host-callable entry points for verifying `Example.Main` from JavaScript.
-- |
-- | The wasm export ABI is `i32 -> … -> i32`, so neither an `Expr` argument nor a
-- | `String` result can cross the boundary directly. We therefore expose nullary
-- | `Int` entry points: `eval*` return the evaluated number, and `print*` compare
-- | `printExpr`'s output against the expected rendering *inside* wasm, returning
-- | `1`/`0` (the same trick the string e2e tests use).
module Example.MainCheck where

import Prelude

import Example.Main (eval, printExpr, testExpr1, testExpr2)

evalTest1 :: Int
evalTest1 = eval testExpr1 -- 1 + 2 * (-3) = -5

evalTest2 :: Int
evalTest2 = eval testExpr2 -- 3 * 5 - 2 + 4 * (2 + 3) = 33

printTest1 :: Int
printTest1 = if printExpr testExpr1 == "1 + 2 * -3" then 1 else 0

printTest2 :: Int
printTest2 = if printExpr testExpr2 == "3 * 5 - 2 + 4 * (2 + 3)" then 1 else 0
