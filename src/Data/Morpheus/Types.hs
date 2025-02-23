module Data.Morpheus.Types
  ( ScalarValue(..)
  , (::->)
  , (::->>)
  , Resolver(..)
  , ID(..)
  , GQLRoot(..)
  , GQLType(..)
  , GQLScalar(..)
  , GQLMutation
  , GQLQuery
  , GQLSubscription
  , GQLArgs
  , withEffect
  ) where

import           Data.Morpheus.Types.GQLArgs        (GQLArgs)
import           Data.Morpheus.Types.GQLOperator    (GQLMutation, GQLQuery, GQLSubscription)
import           Data.Morpheus.Types.GQLScalar      (GQLScalar (..))
import           Data.Morpheus.Types.GQLType        (GQLType (..))
import           Data.Morpheus.Types.ID             (ID (..))
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..))
import           Data.Morpheus.Types.Resolver       ((::->), (::->>), Resolver (..), WithEffect (..))
import           Data.Morpheus.Types.Types          (GQLRoot (..))
import           Data.Text                          (Text)

withEffect :: [Text] -> Either String a -> Either String (WithEffect a)
withEffect channels v = WithEffect channels <$> v
