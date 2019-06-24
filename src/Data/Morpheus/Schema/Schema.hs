{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Schema
  ( initSchema
  , findType
  , Schema
  , Type
  ) where

import           Data.Morpheus.Kind                                (KIND, OBJECT)
import           Data.Morpheus.Schema.Directive                    (Directive)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (Type, createObjectType, renderType)
import           Data.Morpheus.Types.Internal.Data                 (DataOutputObject, DataTypeLib (..), allDataTypes)
import           Data.Text                                         (Text)
import           GHC.Generics                                      (Generic)

type instance KIND (Schema m) = OBJECT

data Schema m = Schema
  { types            :: [Type m]
  , queryType        :: Type m
  , mutationType     :: Maybe (Type m)
  , subscriptionType :: Maybe (Type m)
  , directives       :: [Directive m]
  } deriving (Generic)

convertTypes :: Monad m => DataTypeLib -> [Type m]
convertTypes lib' = map renderType (allDataTypes lib')

buildSchemaLinkType :: Monad m => (Text, DataOutputObject) -> Type m
buildSchemaLinkType (key', _) = createObjectType key' "" $ Just []

findType :: Monad m => Text -> DataTypeLib -> Maybe (Type m)
findType name lib = renderType . (name, ) <$> lookup name (allDataTypes lib)

initSchema :: Monad m => DataTypeLib -> Schema m
initSchema types' =
  Schema
    { types = convertTypes types'
    , queryType = buildSchemaLinkType $ query types'
    , mutationType = buildSchemaLinkType <$> mutation types'
    , subscriptionType = buildSchemaLinkType <$> subscription types'
    , directives = []
    }
