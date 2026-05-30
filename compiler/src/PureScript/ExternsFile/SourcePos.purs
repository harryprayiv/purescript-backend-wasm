module PureScript.ExternsFile.SourcePos where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import PureScript.ExternsFile.Decoder.Class (class Decode)
import PureScript.ExternsFile.Decoder.Generic (genericDecoder)

data SourcePos = SourcePos Int Int -- line, column

derive instance eqSourcePos :: Eq SourcePos
derive instance ordSourcePos :: Ord SourcePos
derive instance genericSourcePos :: Generic SourcePos _
instance showSourcePos :: Show SourcePos where
  show = genericShow

instance decodeSourcePos :: Decode SourcePos where
  decoder = genericDecoder

data SourceSpan = SourceSpan String SourcePos SourcePos

derive instance eqSourceSpan :: Eq SourceSpan
derive instance ordSourceSpan :: Ord SourceSpan
derive instance genericSourceSpan :: Generic SourceSpan _
instance showSourceSpan :: Show SourceSpan where
  show = genericShow

instance decodeSourceSpan :: Decode SourceSpan where
  decoder = genericDecoder

type SourceAnn = Tuple SourceSpan (Array Comment)

data Comment
  = LineComment String
  | BlockComment String

derive instance eqComment :: Eq Comment
derive instance ordComment :: Ord Comment
derive instance genericComment :: Generic Comment _
instance showComment :: Show Comment where
  show = genericShow

instance decodeComment :: Decode Comment where
  decoder = genericDecoder
