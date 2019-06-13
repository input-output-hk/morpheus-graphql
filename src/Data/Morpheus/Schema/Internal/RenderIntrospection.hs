{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Internal.RenderIntrospection
  ( Type
  , Field
  , InputValue
  , createObjectType
  , typeFromObject
  , typeFromInputObject
  , typeFromLeaf
  , typeFromUnion
  ) where

import           Data.Morpheus.Schema.EnumValue    (EnumValue, createEnumValue)
import qualified Data.Morpheus.Schema.Field        as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue   as IN (InputValue (..), createInputValueWith)
import           Data.Morpheus.Schema.Type         (Type (..))
import           Data.Morpheus.Schema.TypeKind     (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data (DataField (..), DataInputField, DataInputObject, DataLeaf (..),
                                                    DataOutputField, DataOutputObject, DataType (..),
                                                    DataTypeWrapper (..), DataUnion)
import           Data.Morpheus.Types.Resolver      (Resolver, QUERY)
import           Data.Text                         (Text)

type InputValue m = IN.InputValue (Type m)

type Field m = F.Field (Type m)

inputValueFromArg :: Monad m => (Text, DataInputField) -> InputValue m
inputValueFromArg (key', input') = IN.createInputValueWith key' (createInputObjectType input')

createInputObjectType :: Monad m => DataInputField -> Type m
createInputObjectType field' = wrap field' $ createType (fieldKind field') (fieldType field') "" []

wrap :: Monad m => DataField a -> Type m -> Type m
wrap field' = wrapRec (fieldTypeWrappers field')

wrapRec :: Monad m => [DataTypeWrapper] -> Type m -> Type m
wrapRec xs type' = foldr wrapByTypeWrapper type' xs

wrapByTypeWrapper :: Monad m => DataTypeWrapper -> Type m -> Type m
wrapByTypeWrapper ListType    = wrapAs LIST
wrapByTypeWrapper NonNullType = wrapAs NON_NULL

fieldFromObjectField :: Monad m => (Text, DataOutputField) -> Field m
fieldFromObjectField (key', field'@DataField {fieldType = type', fieldKind = kind', fieldArgs = args'}) =
  F.createFieldWith key' (wrap field' $ createType kind' type' "" []) (map inputValueFromArg args')

typeFromLeaf :: Monad m => (Text, DataLeaf) -> Type m
typeFromLeaf (key', LeafScalar DataType {typeDescription = desc'}) = createLeafType SCALAR key' desc' []
typeFromLeaf (key', LeafEnum DataType {typeDescription = desc', typeData = tags'}) =
  createLeafType ENUM key' desc' (map createEnumValue tags')

resolveNothing :: Monad m => Resolver m QUERY a (Maybe b)
resolveNothing = return Nothing

resolveMaybeList :: Monad m => [b] -> Resolver m QUERY  a (Maybe [b])
resolveMaybeList list' = return (Just list')

createLeafType :: Monad m => TypeKind -> Text -> Text -> [EnumValue] -> Type m
createLeafType kind' name' desc' enums' =
  Type
    { kind = kind'
    , name = Just name'
    , description = Just desc'
    , fields = resolveNothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = resolveMaybeList enums'
    , inputFields = Nothing
    }

typeFromUnion :: Monad m => (Text, DataUnion) -> Type m
typeFromUnion (name', fields') =
  Type
    { kind = UNION
    , name = Just name'
    , description = Just "TODO"
    , fields = resolveNothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Just (map (\x -> createObjectType (fieldType x) "" []) fields')
    , enumValues = return Nothing
    , inputFields = Nothing
    }

typeFromObject :: Monad m => (Text, DataOutputObject) -> Type m
typeFromObject (key', DataType {typeData = fields', typeDescription = description'}) =
  createObjectType key' description' (map fieldFromObjectField fields')

typeFromInputObject :: Monad m => (Text, DataInputObject) -> Type m
typeFromInputObject (key', DataType {typeData = fields', typeDescription = description'}) =
  createInputObject key' description' (map inputValueFromArg fields')

createObjectType :: Monad m => Text -> Text -> [Field m] -> Type m
createObjectType = createType OBJECT

createInputObject :: Monad m => Text -> Text -> [InputValue m] -> Type m
createInputObject name' desc' fields' =
  Type
    { kind = INPUT_OBJECT
    , name = Just name'
    , description = Just desc'
    , fields = resolveMaybeList []
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = resolveNothing
    , inputFields = Just fields'
    }

createType :: Monad m => TypeKind -> Text -> Text -> [Field m] -> Type m
createType kind' name' desc' fields' =
  Type
    { kind = kind'
    , name = Just name'
    , description = Just desc'
    , fields = resolveMaybeList fields'
    , ofType = Nothing
    , interfaces = Just []
    , possibleTypes = Just []
    , enumValues = resolveMaybeList []
    , inputFields = Just []
    }

wrapAs :: Monad m => TypeKind -> Type m -> Type m
wrapAs kind' contentType =
  Type
    { kind = kind'
    , name = Nothing
    , description = Nothing
    , fields = resolveNothing
    , ofType = Just contentType
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = resolveNothing
    , inputFields = Nothing
    }
