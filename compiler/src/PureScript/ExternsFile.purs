module PureScript.ExternsFile
  ( Associativity(..)
  , ExternsDeclaration(..)
  , ExternsFile(..)
  , ExternsFixity(..)
  , ExternsImport(..)
  , ExternsTypeFixity(..)
  , Fixity(..)
  , ImportDeclarationType(..)
  , Precedence
  , identOfExternsDeclaration
  , module Ext
  ) where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\))
import PureScript.ExternsFile.Declarations (DeclarationRef(..)) as Ext
import PureScript.ExternsFile.Decoder.Class (class Decode)
import PureScript.ExternsFile.Decoder.Generic (genericDecoder)
import PureScript.ExternsFile.Names (Ident(..), ModuleName, NameSource, OpName, ProperName(..), Qualified)
import PureScript.ExternsFile.SourcePos (SourceSpan)
import PureScript.ExternsFile.Types (ChainId, DataDeclType, FunctionalDependency, SourceConstraint, SourceType, TypeKind)

data ExternsFile = ExternsFile
  String -- efVersion
  ModuleName -- efModuleName
  (Array Ext.DeclarationRef) -- efExports
  (Array ExternsImport) -- efImports
  (Array ExternsFixity) -- efFixities
  (Array ExternsTypeFixity) -- efTypeFixities
  (Array ExternsDeclaration) -- efDeclarations
  SourceSpan -- efSourceSpan

derive instance genericExternsFile :: Generic ExternsFile _
instance showExternsFile :: Show ExternsFile where
  show = genericShow

instance decodeExternsFile :: Decode ExternsFile where
  decoder = genericDecoder

data ImportDeclarationType
  = Implicit
  | Explicit (Array Ext.DeclarationRef)
  | Hiding (Array Ext.DeclarationRef)

derive instance eqImportDeclarationType :: Eq ImportDeclarationType
derive instance ordImportDeclarationType :: Ord ImportDeclarationType
derive instance genericImportDeclarationType :: Generic ImportDeclarationType _
instance showImportDeclarationType :: Show ImportDeclarationType where
  show = genericShow

instance decodeImportDeclarationType :: Decode ImportDeclarationType where
  decoder = genericDecoder

data ExternsImport = ExternsImport
  ModuleName
  ImportDeclarationType
  (Maybe ModuleName)

derive instance eqExternsImport :: Eq ExternsImport
derive instance ordExternsImport :: Ord ExternsImport
derive instance genericExternsImport :: Generic ExternsImport _
instance showExternsImport :: Show ExternsImport where
  show = genericShow

instance decodeExternsImport :: Decode ExternsImport where
  decoder = genericDecoder

type Precedence = Int

data Associativity
  = Infix
  | Infixl
  | Infixr

derive instance eqAssociativity :: Eq Associativity
derive instance ordAssociativity :: Ord Associativity
derive instance genericAssociativity :: Generic Associativity _
instance showAssociativity :: Show Associativity where
  show = genericShow

instance decodeAssociativity :: Decode Associativity where
  decoder = genericDecoder

data Fixity = Fixity Associativity Precedence

derive instance eqFixity :: Eq Fixity
derive instance ordFixity :: Ord Fixity
derive instance genericFixity :: Generic Fixity _
instance showFixity :: Show Fixity where
  show = genericShow

data ExternsFixity = ExternsFixity
  Associativity
  Precedence
  OpName
  (Qualified (Either Ident ProperName))

derive instance eqExternsFixity :: Eq ExternsFixity
derive instance ordExternsFixity :: Ord ExternsFixity
derive instance genericExternsFixity :: Generic ExternsFixity _
instance showExternsFixity :: Show ExternsFixity where
  show = genericShow

instance decodeExternsFixity :: Decode ExternsFixity where
  decoder = genericDecoder

data ExternsTypeFixity = ExternsTypeFixity Associativity Precedence OpName (Qualified ProperName)

derive instance eqExternsTypeFixity :: Eq ExternsTypeFixity
derive instance ordExternsTypeFixity :: Ord ExternsTypeFixity
derive instance genericExternsTypeFixity :: Generic ExternsTypeFixity _
instance showExternsTypeFixity :: Show ExternsTypeFixity where
  show = genericShow

instance decodeExternsTypeFixity :: Decode ExternsTypeFixity where
  decoder = genericDecoder

data ExternsDeclaration
  = EDType
      ProperName -- edTypeName
      SourceType -- edTypeKind
      TypeKind -- edTypeDeclarationKind
  | EDTypeSynonym
      ProperName -- edTypeSynonymName
      (Array (String /\ Maybe SourceType)) -- edTypeSynonymArguments
      SourceType -- edTypeSynonymType
  | EDDataConstructor
      ProperName -- edDataCtorName
      DataDeclType -- edDataCtorOrigin
      ProperName -- edDataCtorTypeCtor
      SourceType -- edDataCtorType
      (Array Ident) -- edDataCtorFields
  | EDValue
      Ident -- edValueName
      SourceType -- edValueType
  | EDClass
      ProperName -- edClassName
      (Array (String /\ Maybe SourceType)) -- edClassTypeArguments
      (Array (Ident /\ SourceType)) -- edClassMembers
      (Array SourceConstraint) -- edClassConstraints
      (Array FunctionalDependency) -- edFunctionalDependencies
      Boolean -- edIsEmpty
  | EDInstance
      (Qualified ProperName) -- edInstanceClassName
      Ident -- edInstanceName
      (Array (Tuple String SourceType)) -- edInstanceForAll
      (Array SourceType) -- edInstanceKinds
      (Array SourceType) -- edInstanceTypes
      (Maybe (Array SourceConstraint)) -- edInstanceConstraints
      (Maybe ChainId) -- edInstanceChain
      Int -- edInstanceChainIndex
      NameSource -- edInstanceNameSource
      SourceSpan -- edInstanceSourceSpan

derive instance genericExternsDeclaration :: Generic ExternsDeclaration _
instance showExternsDeclaration :: Show ExternsDeclaration where
  show = genericShow

instance decodeExternsDeclaration :: Decode ExternsDeclaration where
  decoder = genericDecoder

identOfExternsDeclaration :: ExternsDeclaration -> Ident
identOfExternsDeclaration = case _ of
  EDType pn _ _ -> properNameIdent pn
  EDTypeSynonym pn _ _ -> properNameIdent pn
  EDDataConstructor pn _ _ _ _ -> properNameIdent pn
  EDClass pn _ _ _ _ _ -> properNameIdent pn
  EDInstance _ ident _ _ _ _ _ _ _ _ -> ident
  EDValue ident _ -> ident
  where
  properNameIdent (ProperName ident) = Ident ident
