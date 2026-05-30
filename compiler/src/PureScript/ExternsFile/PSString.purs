module PureScript.ExternsFile.PSString
  ( PSString(..)
  , toString
  ) where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import PureScript.ExternsFile.Decoder.Class (class Decode)
import PureScript.ExternsFile.Decoder.Newtype (newtypeDecoder)

-- | A PureScript string as the array of UTF-16 code units the compiler stores.
newtype PSString = PSString (Array Int)

derive instance newtypePSString :: Newtype PSString _
derive newtype instance eqPSString :: Eq PSString
derive newtype instance ordPSString :: Ord PSString

instance showPSString :: Show PSString where
  show _ = "(PSString <...>)"

instance decodePSString :: Decode PSString where
  decoder = newtypeDecoder

-- | Render a `PSString`'s code units back into a `String`. This is the safe
-- | wrapper over `unsafeUTF16ToString`; the unsafe primitive is kept private so
-- | callers cannot feed it arbitrary `Array Int` values.
toString :: PSString -> String
toString = unsafeUTF16ToString <<< unwrap

foreign import unsafeUTF16ToString :: Array Int -> String
