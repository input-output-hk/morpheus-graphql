{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Resolve.Encode
  ( ObjectFieldResolvers(..)
  , resolveBySelection
  , resolveBySelectionM
  , resolversBy
  ) where

import           Control.Monad.Trans                            (lift)
import           Control.Monad.Trans.Except
import           Data.Morpheus.Error.Internal                   (internalErrorIO)
import           Data.Morpheus.Error.Selection                  (fieldNotResolved, subfieldsNotSelected)
import           Data.Morpheus.Kind                             (ENUM, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Resolve.Generics.DeriveResolvers (ObjectFieldResolvers (..), UnionResolvers (..),
                                                                 lookupSelectionByType, resolveBySelection,
                                                                 resolveBySelectionM, resolversBy)
import           Data.Morpheus.Resolve.Generics.EnumRep         (EnumRep (..))
import qualified Data.Morpheus.Types.GQLArgs                    as Args (GQLArgs (..))
import           Data.Morpheus.Types.GQLScalar                  (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                    (GQLType (__typeName))
import           Data.Morpheus.Types.Internal.AST.Selection     (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Base              (Position)
import           Data.Morpheus.Types.Internal.Validation        (ResolveT, failResolveT)
import           Data.Morpheus.Types.Internal.Value             (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver                   (Resolver (..), WithEffect (..), QUERY, MUTATION)
import           Data.Proxy                                     (Proxy (..))
import           Data.Text                                      (Text, pack)
import           GHC.Generics

type ObjectConstraint m a b = (Generic a, GQLType a, ObjectFieldResolvers m (Rep a) b)

type UnionConstraint m a res = (Generic a, GQLType a, UnionResolvers m (Rep a) res)

type EnumConstraint a = (Generic a, EnumRep (Rep a))

type MResult = WithEffect Value

type QueryResult = Value

newtype WithGQLKind a b = WithGQLKind
  { resolverValue :: a
  }

type GQLKindOf a = WithGQLKind a (KIND a)

encode ::
     forall m a b. Encoder m a (KIND a) b
  => a
  -> (Text, Selection)
  -> ResolveT m b
encode resolver = __encode (WithGQLKind resolver :: GQLKindOf a)

class Encoder m a kind toValue where
  __encode :: WithGQLKind a kind -> (Text, Selection) -> ResolveT m toValue

--
-- SCALAR
--
instance (Monad m, GQLScalar a) => Encoder m a SCALAR QueryResult where
  __encode = pure . pure . Scalar . serialize . resolverValue

instance (Monad m, GQLScalar a) => Encoder m a SCALAR MResult where
  __encode value selection = pure <$> __encode value selection

--
-- ENUM
--
instance (Monad m, EnumConstraint a) => Encoder m a ENUM QueryResult where
  __encode = pure . pure . Scalar . String . encodeRep . from . resolverValue

instance (Monad m, EnumConstraint a) => Encoder m a ENUM MResult where
  __encode value selection = pure <$> __encode value selection

--
--  OBJECTS
--
instance (Monad m, ObjectConstraint m a QueryResult) => Encoder m a OBJECT QueryResult where
  __encode (WithGQLKind value) (_, Selection {selectionRec = SelectionSet selection'}) =
    resolveBySelection selection' (__typenameResolver : resolversBy value)
    where
      __typenameResolver = ("__typename", const $ return $ Scalar $ String $ __typeName (Proxy @a))
  __encode _ (key, Selection {selectionPosition}) = failResolveT $ subfieldsNotSelected key "" selectionPosition

instance (Monad m, ObjectConstraint m a MResult) => Encoder m a OBJECT MResult where
  __encode (WithGQLKind value) (_, Selection {selectionRec = SelectionSet selection'}) =
    resolveBySelectionM selection' (__typenameResolver : resolversBy value)
    where
      __typenameResolver = ("__typename", const $ return $ return $ Scalar $ String $ __typeName (Proxy @a))
  __encode _ (key, Selection {selectionPosition}) = failResolveT $ subfieldsNotSelected key "" selectionPosition

instance Encoder m a (KIND a) res => ObjectFieldResolvers m (K1 s a) res where
  objectFieldResolvers key' (K1 src) = [(key', encode src)]

-- | Resolves and encodes UNION,
-- Handles all operators: Query, Mutation and Subscription,
instance (Monad m, UnionConstraint m a res) => Encoder m a UNION res where
  __encode (WithGQLKind value) (key', sel@Selection {selectionRec = UnionSelection selections'}) =
    resolver (key', sel {selectionRec = SelectionSet (lookupSelectionByType type' selections')})
    where
      (type', resolver) = unionResolvers (from value)
  __encode _ _ = internalErrorIO "union Resolver only should recieve UnionSelection"

instance (GQLType a, Encoder m a (KIND a) res) => UnionResolvers m (K1 s a) res where
  unionResolvers (K1 src) = (__typeName (Proxy @a), encode src)

--
--  RESOLVER: ::-> and ::->>
--
-- | Handles all operators: Query, Mutation and Subscription,
-- if you use it with Mutation or Subscription all effects inside will be lost
instance (Monad m, Encoder m a (KIND a) res, Args.GQLArgs p) => Encoder m (Resolver m QUERY p a) WRAPPER res where
  __encode (WithGQLKind (Resolver resolver)) selection'@(key', Selection {selectionArguments, selectionPosition}) = do
    args <- ExceptT $ pure $ Args.decode selectionArguments
    liftResolver selectionPosition key' (resolver args) >>= (`encode` selection')

-- | resolver with effect, concatenates sideEffects of child resolvers
instance (Monad m, Encoder m a (KIND a) MResult, Args.GQLArgs p) => Encoder m (Resolver m MUTATION p a) WRAPPER MResult where
  __encode (WithGQLKind (Resolver resolver)) selection'@(key', Selection {selectionArguments, selectionPosition}) = do
    args <- ExceptT $ pure $ Args.decode selectionArguments
    WithEffect effects1 value1 <- liftResolver selectionPosition key' (resolver args)
    WithEffect effects2 value2 <- __encode (WithGQLKind value1 :: GQLKindOf a) selection'
    return $ WithEffect (effects1 ++ effects2) value2

liftResolver :: Monad m => Position -> Text -> m (Either String a) -> ResolveT m a
liftResolver position' typeName' x = do
  result <- lift x
  case result of
    Left message' -> failResolveT $ fieldNotResolved position' typeName' (pack message')
    Right value   -> pure value

--
-- MAYBE
--
instance (Monad m, Encoder m a (KIND a) QueryResult) => Encoder m (Maybe a) WRAPPER QueryResult where
  __encode (WithGQLKind Nothing)      = const $ pure Null
  __encode (WithGQLKind (Just value)) = encode value

instance (Monad m, Encoder m a (KIND a) MResult) => Encoder m (Maybe a) WRAPPER MResult where
  __encode (WithGQLKind Nothing)      = const $ pure $ pure Null
  __encode (WithGQLKind (Just value)) = encode value

--
-- LIST
--
instance (Monad m, Encoder m a (KIND a) QueryResult) => Encoder m [a] WRAPPER QueryResult where
  __encode list query = List <$> mapGQLList list query

instance (Monad m, Encoder m a (KIND a) MResult) => Encoder m [a] WRAPPER MResult where
  __encode list query = do
    value' <- mapGQLList list query
    return $ WithEffect (concatMap resultEffects value') (List (map resultValue value'))

mapGQLList ::
     forall m a b. (Monad m, Encoder m a (KIND a) b)
  => GQLKindOf [a]
  -> (Text, Selection)
  -> ResolveT m [b]
mapGQLList (WithGQLKind list) query = mapM (`__encode` query) (map WithGQLKind list :: [GQLKindOf a])
