{-# LANGUAGE KindSignatures #-}
module Data.Morpheus.Types.Types
  ( GQLQueryRoot(..)
  , Variables
  , GQLRootResolver(..)
  , SubscriptionResolver(..)
  ) where

import           Data.Map                                      (Map)
import           Data.Morpheus.Types.Internal.AST.Operator     (RawOperator)
import           Data.Morpheus.Types.Internal.AST.RawSelection (FragmentLib)
import           Data.Morpheus.Types.Internal.Base             (Key)
import           Data.Morpheus.Types.Internal.Validation       (ResolveT)
import           Data.Morpheus.Types.Internal.Value            (Value)

type Variables = Map Key Value

newtype SubscriptionResolver m = SubscriptionResolver
  { unpackSubscriptionResolver :: () -> ResolveT m Value
  }

instance Show (SubscriptionResolver m) where
  show = const "SubscriptionResolver"

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , queryBody      :: RawOperator
  , inputVariables :: [(Key, Value)]
  }

data GQLRootResolver (f :: * -> *) q m s = GQLRootResolver
  { queryResolver        :: q
  , mutationResolver     :: m
  , subscriptionResolver :: s
  }
