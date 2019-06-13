module Data.Morpheus.Types
  ( ScalarValue(..)
  , (::->)
  , (::->>)
  , Resolver(..)
  , ResolveT
  , ResolveCon
  , ID(..)
  , GQLRootResolver(..)
  , GQLType(..)
  , asObjectType
  , GQLScalar(..)
  , GQLRequest(..)
  , GQLResponse(..)
  , GQLArgs
  , QUERY
  , MUTATION
  , withEffect
  , WithEffect
  , InputTypeRouter(..)
  , TypeKind(..)
  , _field
  , _decode
  , _introspect
  ) where

import           Data.Morpheus.Resolve.Decode            (InputTypeRouter (..), _introspect, _decode, _field)
import           Data.Morpheus.Resolve.Resolve           (ResolveCon)
import           Data.Morpheus.Types.GQLArgs             (GQLArgs)
import           Data.Morpheus.Types.GQLScalar           (GQLScalar (..))
import           Data.Morpheus.Types.GQLType             (GQLType (..), asObjectType)
import           Data.Morpheus.Types.ID                  (ID (..))
import           Data.Morpheus.Schema.TypeKind (TypeKind(..))
import           Data.Morpheus.Types.Internal.Validation (ResolveT)
import           Data.Morpheus.Types.Internal.Value      (ScalarValue (..))
import           Data.Morpheus.Types.Request             (GQLRequest (..))
import           Data.Morpheus.Types.Resolver            ((::->), (::->>), MUTATION, QUERY, Resolver (..),
                                                          WithEffect (..))
import           Data.Morpheus.Types.Response            (GQLResponse (..))
import           Data.Morpheus.Types.Types               (GQLRootResolver (..))
import           Data.Text                               (Text)

withEffect :: [Text] -> Either String a -> Either String (WithEffect a)
withEffect channels v = WithEffect channels <$> v
