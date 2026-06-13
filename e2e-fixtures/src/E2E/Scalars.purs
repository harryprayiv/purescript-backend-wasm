-- | The scalar-Int E2E fixture (built standalone by `purs-wasm build -e E2E.Scalars`,
-- | asserted by `Test.E2E.Cli.Scalars`).
-- |
-- | It deliberately uses module-local `foreign import` primitives (mapped to
-- | i32 intrinsics by the backend's `ForeignProvider`) instead of `+`/`*`, so
-- | it pulls in no type-class dictionaries, records, closures, or pattern
-- | matching — only top-level functions, integer literals, and saturated calls.
module E2E.Scalars where

foreign import intAdd :: Int -> Int -> Int
foreign import intMul :: Int -> Int -> Int

double :: Int -> Int
double x = intAdd x x

quad :: Int -> Int
quad x = double (double x)

sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = intAdd (intMul x x) (intMul y y)

five :: Int
five = intAdd 2 3
