module Example.Main where

import Prelude

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Lit Int

eval :: Expr -> Int
eval = case _ of
  Add x y -> eval x + eval y
  Mul x y -> eval x * eval y
  Neg x -> negate (eval x)
  Lit n -> n

printExpr :: Expr -> String
printExpr e = printWithParentheses 0 e
  where
  printWithParentheses :: Int -> Expr -> String
  printWithParentheses prec e = case e of
    Add x y
      | Neg y' <- y -> if prec > 1 then "(" <> printWithParentheses 1 x <> " - " <> printWithParentheses 3 y' <> ")" else printWithParentheses 1 x <> " - " <> printWithParentheses 3 y'
      | otherwise -> if prec > 1 then "(" <> printWithParentheses 1 x <> " + " <> printWithParentheses 1 y <> ")" else printWithParentheses 1 x <> " + " <> printWithParentheses 1 y
    Mul x y -> if prec > 2 then "(" <> printWithParentheses 2 x <> " * " <> printWithParentheses 2 y <> ")" else printWithParentheses 2 x <> " * " <> printWithParentheses 2 y
    Neg x -> if prec > 3 then "(-" <> printWithParentheses 3 x <> ")" else "-" <> printWithParentheses 3 x
    Lit n -> show n

-- 1 + 2 * (-3)
testExpr1 :: Expr
testExpr1 = Add (Lit 1) (Mul (Lit 2) (Neg (Lit 3)))

-- 3 * 5 - 2 + 4 * (2 + 3)
testExpr2 :: Expr
testExpr2 = Add (Add (Mul (Lit 3) (Lit 5)) (Neg (Lit 2))) (Mul (Lit 4) (Add (Lit 2) (Lit 3)))