module PureScript.ExternsFile.Names where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import PureScript.ExternsFile.Decoder.Class (class Decode, decoder)
import PureScript.ExternsFile.Decoder.Generic (genericDecoder)
import PureScript.ExternsFile.Decoder.Newtype (newtypeDecoder)
import PureScript.ExternsFile.SourcePos (SourcePos)

newtype ModuleName = ModuleName String

derive instance newtypeModuleName :: Newtype ModuleName _
derive instance eqModuleName :: Eq ModuleName
derive instance ordModuleName :: Ord ModuleName
instance showModuleName :: Show ModuleName where
  show (ModuleName mn) = "(ModuleName " <> mn <> ")"

-- A ModuleName is serialized as a bare string, not a wrapped newtype.
instance decodeModuleName :: Decode ModuleName where
  decoder = ModuleName <$> decoder

newtype ProperName = ProperName String

derive instance newtypeProperName :: Newtype ProperName _
derive instance eqProperName :: Eq ProperName
derive instance ordProperName :: Ord ProperName
instance showProperName :: Show ProperName where
  show (ProperName pn) = "(ProperName " <> pn <> ")"

instance decodeProperName :: Decode ProperName where
  decoder = newtypeDecoder

newtype OpName = OpName String

derive instance newtypeOpName :: Newtype OpName _
derive instance eqOpName :: Eq OpName
derive instance ordOpName :: Ord OpName
instance showOpName :: Show OpName where
  show (OpName on) = "(OpName " <> on <> ")"

instance decodeOpName :: Decode OpName where
  decoder = newtypeDecoder

newtype Ident = Ident String

derive instance newtypeIdent :: Newtype Ident _
derive instance eqIdent :: Eq Ident
derive instance ordIdent :: Ord Ident
instance showIdent :: Show Ident where
  show (Ident id) = "(Ident " <> id <> ")"

instance decodeIdent :: Decode Ident where
  decoder = newtypeDecoder

data QualifiedBy
  = BySourcePos SourcePos
  | ByModuleName ModuleName

derive instance eqQualifiedBy :: Eq QualifiedBy
derive instance ordQualifiedBy :: Ord QualifiedBy
derive instance genericQualifiedBy :: Generic QualifiedBy _
instance showQualifiedBy :: Show QualifiedBy where
  show = genericShow

instance decodeQualifiedBy :: Decode QualifiedBy where
  decoder = genericDecoder

data Qualified a = Qualified QualifiedBy a

derive instance eqQualified :: Eq a => Eq (Qualified a)
derive instance ordQualified :: Ord a => Ord (Qualified a)
derive instance genericQualified :: Generic (Qualified a) _
instance showQualified :: Show a => Show (Qualified a) where
  show = genericShow

instance decodeQualified :: Decode a => Decode (Qualified a) where
  decoder = genericDecoder

data NameSource = UserNamed | CompilerNamed

derive instance eqNameSource :: Eq NameSource
derive instance ordNameSource :: Ord NameSource
derive instance genericNameSource :: Generic NameSource _
instance showNameSource :: Show NameSource where
  show = genericShow

instance decodeNameSource :: Decode NameSource where
  decoder = genericDecoder
