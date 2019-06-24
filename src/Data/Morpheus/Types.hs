-- | GQL Types
module Data.Morpheus.Types
  ( ScalarValue(..)
  , (::->)
  , (::->>)
  , Resolver(..)
  , ID(..)
  , GQLType(__typeName, description, updateLib)
  , GQLRootResolver(..)
  , GQLScalar(parseValue, serialize)
  , GQLRequest(..)
  , GQLResponse(..)
  , GQLArgs
  , GQLMutation
  , GQLQuery
  , GQLSubscription
  , WithEffect
  , withEffect
  , QUERY
  , MUTATION
  , Introspect(..)
  , InputType
  , OutputType
  , Context(..)
  , DataField(..)
  , DataTypeWrapper(..)
  ) where

import           Data.Morpheus.Resolve.Introspect   (InputType, Introspect (..), OutputType, Context(..))
import           Data.Morpheus.Types.GQLArgs        (GQLArgs)
import           Data.Morpheus.Types.GQLOperator    (GQLMutation, GQLQuery, GQLSubscription)
import           Data.Morpheus.Types.GQLScalar      (GQLScalar (parseValue, serialize))
import           Data.Morpheus.Types.GQLType        (GQLType (description, updateLib, __typeName))
import           Data.Morpheus.Types.ID             (ID (..))
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..))
import           Data.Morpheus.Types.Internal.Data  (DataField(..),DataTypeWrapper(..))
import           Data.Morpheus.Types.Request        (GQLRequest (..))
import           Data.Morpheus.Types.Resolver       ((::->), (::->>), MUTATION, QUERY, Resolver (..), WithEffect (..))
import           Data.Morpheus.Types.Response       (GQLResponse (..))
import           Data.Morpheus.Types.Types          (GQLRootResolver (..))
import           Data.Text                          (Text)

-- | used in mutation or subscription resolver , adds effect to normal resolver
withEffect :: [Text] -> Either String a -> Either String (WithEffect a)
withEffect channels v = WithEffect channels <$> v
