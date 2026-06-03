-- | Benchmark programs for the wasm backend. Each entry is an `Int -> Int` (the
-- | i32 export ABI) taking a workload size and returning a checksum / result, so
-- | the runner can both time it and sanity-check correctness. All are
-- | self-contained (user-defined ADTs + `Prelude` only — no external packages), so
-- | they run on the current backend and form a stable baseline to measure
-- | optimization against.
module Bench.Main where

import Prelude

-- A self-contained linked list (no `arrays` / `lists` package needed).
data IntList = Nil | Cons Int IntList

-- 1. fib — tree recursion + Int arithmetic.
fib :: Int -> Int
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

-- 2. sumLoop — a tight numeric loop whose `+` / `*` / `>` all go through Prelude
--    dictionaries (the prime target for dictionary elimination). Tail-recursive.
sumLoop :: Int -> Int
sumLoop n = go 0 1
  where
  go acc i = if i > n then acc else go (acc + i * i) (i + 1)

-- 3. quicksort — list ADT: predicate closures, `Ord` comparisons, heavy Cons
--    allocation. Returns 1 iff the result is sorted (forces full evaluation).
append :: IntList -> IntList -> IntList
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

filterBy :: (Int -> Boolean) -> IntList -> IntList
filterBy pred = case _ of
  Nil -> Nil
  Cons x xs -> if pred x then Cons x (filterBy pred xs) else filterBy pred xs

quicksort :: IntList -> IntList
quicksort = case _ of
  Nil -> Nil
  Cons p rest -> append (quicksort (filterBy (\x -> x <= p) rest)) (Cons p (quicksort (filterBy (\x -> x > p) rest)))

buildList :: Int -> Int -> IntList
buildList k s = if k == 0 then Nil else Cons s (buildList (k - 1) (s * 1103515245 + 12345))

isSorted :: IntList -> Boolean
isSorted = case _ of
  Nil -> true
  Cons _ Nil -> true
  Cons x (Cons y rest) -> if x <= y then isSorted (Cons y rest) else false

qsort :: Int -> Int
qsort n = if isSorted (quicksort (buildList n 1)) then 1 else 0

-- 4. N-Queens — backtracking; returns the number of solutions on an n×n board.
-- (`placeAt` keeps the per-column `if` in its own tail position — a style choice;
-- the backend handles a `case` / `if` in argument position too.)
nqueens :: Int -> Int
nqueens n = go Nil 0
  where
  go placed row = if row == n then 1 else tryCols 0 placed row
  tryCols col placed row =
    if col == n then 0
    else placeAt col placed row + tryCols (col + 1) placed row
  placeAt col placed row =
    if safe col placed 1 then go (Cons col placed) (row + 1) else 0
  safe col placed dist = case placed of
    Nil -> true
    Cons c rest ->
      if c == col || c == col - dist || c == col + dist then false
      else safe col rest (dist + 1)

-- 5/6. Binary tree traversals.
data Tree = Leaf | Node Int Tree Tree
data TreeQ = QNil | QCons Tree TreeQ

mkTree :: Int -> Int -> Tree
mkTree depth v = if depth == 0 then Leaf else Node v (mkTree (depth - 1) (v + v)) (mkTree (depth - 1) (v + v + 1))

dfsSum :: Tree -> Int
dfsSum = case _ of
  Leaf -> 0
  Node v l r -> v + dfsSum l + dfsSum r

-- depth-first traversal of a balanced tree of the given depth; sums node values.
bintreeDfs :: Int -> Int
bintreeDfs depth = dfsSum (mkTree depth 1)

appendQ :: TreeQ -> TreeQ -> TreeQ
appendQ QNil ys = ys
appendQ (QCons x xs) ys = QCons x (appendQ xs ys)

bfsSum :: TreeQ -> Int
bfsSum = case _ of
  QNil -> 0
  QCons t rest -> case t of
    Leaf -> bfsSum rest
    Node v l r -> v + bfsSum (appendQ rest (QCons l (QCons r QNil)))

-- breadth-first traversal (list-queue) of a balanced tree of the given depth.
bintreeBfs :: Int -> Int
bintreeBfs depth = bfsSum (QCons (mkTree depth 1) QNil)

-- 7. mapFold — higher-order list processing over a **polymorphic** list `List a`.
-- `mapList` / `foldlList` are the standard map / left-fold shape: a recursive
-- function with a *static* function argument (passed unchanged through the
-- recursion). Applying them to closures stresses closure allocation and indirect
-- (`call_ref`) application — the target of higher-order specialization. Because
-- `List a` is polymorphic, the element field is a boxed `eqref`, not an unboxed
-- `i32` (front-B field unboxing applies only to *concrete*-scalar fields) — which is
-- the realistic, fair case: JavaScript stores the number natively in the cell too.
data List a = LNil | LCons a (List a)

mapList :: forall a b. (a -> b) -> List a -> List b
mapList f = case _ of
  LNil -> LNil
  LCons x xs -> LCons (f x) (mapList f xs)

foldlList :: forall a b. (b -> a -> b) -> b -> List a -> b
foldlList f acc = case _ of
  LNil -> acc
  LCons x xs -> foldlList f (f acc x) xs

range :: Int -> List Int
range k = if k == 0 then LNil else LCons k (range (k - 1))

-- `iters` left-folds (with a closure) a fixed list that was built and mapped (with
-- a closure) once. `mapList` is not tail-recursive, so the list is kept moderate;
-- the iteration is the tail-recursive `loop`, making the per-element closure
-- application — not stack depth or allocation — the thing being measured.
mapFold :: Int -> Int
mapFold iters = loop iters 0
  where
  base = mapList (\x -> x + 1) (range 2000)
  loop k acc = if k == 0 then acc else loop (k - 1) (foldlList (\a x -> a + x) acc base)
