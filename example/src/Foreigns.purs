-- | A fixture of diverse `foreign import` signatures, for unit-testing the externs
-- | → calling-convention extraction edge cases (ADR 0014). Not an entry module;
-- | only its `externs.cbor` is used (copied into the compiler test fixtures).
module Example.Foreigns where

foreign import addOne :: Int -> Int
foreign import scale :: Int -> Number -> Number
foreign import maxInt :: Int
foreign import toChar :: Int -> Char
foreign import identityF :: forall a. a -> a
foreign import flag :: Boolean
foreign import shout :: String -> String
