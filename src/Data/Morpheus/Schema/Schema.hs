{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Schema.Schema where

import           Data.Morpheus.Kind                                (KIND, OBJECT)
import           Data.Morpheus.Schema.Directive                    (Directive)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (Type, createObjectType, typeFromInputObject,
                                                                    typeFromLeaf, typeFromObject, typeFromUnion)
import           Data.Morpheus.Types.Internal.Data                 (DataOutputObject, DataTypeLib (..))
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
convertTypes lib' =
  [typeFromObject $ query lib'] ++
  typeFromMaybe (mutation lib') ++
  typeFromMaybe (subscription lib') ++
  map typeFromObject (object lib') ++
  map typeFromInputObject (inputObject lib') ++ map typeFromLeaf (leaf lib') ++ map typeFromUnion (union lib')

typeFromMaybe :: Monad m => Maybe (Text, DataOutputObject) -> [Type m]
typeFromMaybe (Just x) = [typeFromObject x]
typeFromMaybe Nothing  = []

buildSchemaLinkType :: Monad m => (Text, DataOutputObject) -> Type m
buildSchemaLinkType (key', _) = createObjectType key' "Query Description" []

initSchema :: Monad m => DataTypeLib -> Schema m
initSchema types' =
  Schema
    { types = convertTypes types'
    , queryType = buildSchemaLinkType $ query types'
    , mutationType = buildSchemaLinkType <$> mutation types'
    , subscriptionType = buildSchemaLinkType <$> subscription types'
    , directives = []
    }
