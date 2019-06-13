{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Resolve.Encode where

import           Control.Monad.Trans                            (lift)
import           Control.Monad.Trans.Except
import           Data.Morpheus.Error.Internal                   (internalErrorT)
import           Data.Morpheus.Error.Selection                  (fieldNotResolved, subfieldsNotSelected)
import           Data.Morpheus.Kind                             (ENUM, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Resolve.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection,
                                                                 resolveBySelectionM, resolversBy)
import           Data.Morpheus.Resolve.Generics.EnumRep         (EnumRep (..))
import           Data.Morpheus.Resolve.Generics.UnionResolvers  (UnionResolvers (..), lookupSelectionByType)
import           Data.Morpheus.Resolve.Internal                 (EncodeObjectConstraint, EncodeUnionConstraint, Encode_,
                                                                 EnumConstraint)
import qualified Data.Morpheus.Types.GQLArgs                    as Args (GQLArgs (..))
import qualified Data.Morpheus.Types.GQLScalar                  as S (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                    (GQLType (..))
import           Data.Morpheus.Types.Internal.AST.Selection     (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Base              (Position)
import           Data.Morpheus.Types.Internal.Validation        (ResolveT, failResolveT)
import           Data.Morpheus.Types.Internal.Value             (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver                   (QUERY, MUTATION, Resolver (..), WithEffect (..))
import           Data.Proxy                                     (Proxy (..))
import           Data.Text                                      (Text, pack)
import           GHC.Generics

type MResult = WithEffect Value

type QueryResult = Value

-- { ENCODE }
class Encoder m a kind toValue where
  __encode :: Proxy kind -> Encode_ m a toValue

_encode ::
     forall m a v. Encoder m a (KIND a) v
  => Encode_ m a v
_encode = __encode (Proxy @(KIND a))

-- Encode Queries
instance (Monad m, S.GQLScalar a, GQLType a) => Encoder m a SCALAR QueryResult where
  __encode _ _ = pure . S.encode

instance (Monad m, EnumConstraint a) => Encoder m a ENUM QueryResult where
  __encode _ _ = pure . Scalar . String . encodeRep . from

instance (Monad m, EncodeObjectConstraint m a QueryResult) => Encoder m a OBJECT QueryResult where
  __encode _ = encodeObject
    where
      encodeObject :: Encode_ m a QueryResult
      encodeObject (_, Selection {selectionRec = SelectionSet selection'}) value =
        resolveBySelection selection' (__typename : resolversBy value)
        where
          __typename = ("__typename", const $ return $ Scalar $ String $typeID (Proxy @a))
      encodeObject (key, Selection {selectionPosition = position'}) _ =
        failResolveT $ subfieldsNotSelected key "" position'

instance (Monad m, EncodeUnionConstraint m a QueryResult) => Encoder m a UNION QueryResult where
  __encode _ = encodeUnion
    where
      encodeUnion :: Encode_ m a QueryResult
      encodeUnion (key', sel@Selection {selectionRec = UnionSelection selections'}) value =
        resolver (key', sel {selectionRec = SelectionSet (lookupSelectionByType type' selections')})
        where
          (type', resolver) = currentResolver (from value)
      encodeUnion _ _ = internalErrorT "union Resolver only should recieve UnionSelection"

instance (Monad m, Encoder m a (KIND a) QueryResult) => Encoder m (Maybe a) WRAPPER QueryResult where
  __encode _ = encodeMaybe Null _encode

instance (Monad m, Encoder m a (KIND a) QueryResult) => Encoder m [a] WRAPPER QueryResult where
  __encode _ = encodeList _encode
    where
      encodeList :: Encode_ m a Value -> Encode_ m [a] Value
      encodeList _ (_, Selection {selectionRec = SelectionField {}}) _ = pure $ List []
      encodeList f query list                                          = List <$> mapM (f query) list

instance (Monad m, Encoder m a (KIND a) QueryResult, Args.GQLArgs p) => Encoder m (Resolver m QUERY p a) WRAPPER QueryResult where
  __encode _ selection'@(key', Selection {selectionArguments = astArgs', selectionPosition = position'}) (Resolver resolver) = do
    args <- ExceptT $ pure $ Args.decode astArgs'
    liftResolver position' key' (resolver args) >>= _encode selection'

-- Encode Mutations and Subscriptions
instance (Monad m, S.GQLScalar a, GQLType a) => Encoder m a SCALAR MResult where
  __encode _ _ = pure . pure . S.encode

instance (Monad m, EnumConstraint a) => Encoder m a ENUM MResult where
  __encode _ _ = pure . pure . Scalar . String . encodeRep . from

instance (Monad m, EncodeObjectConstraint m a MResult) => Encoder m a OBJECT MResult where
  __encode _ = encodeObject
    where
      encodeObject :: Encode_ m a MResult
      encodeObject (_, Selection {selectionRec = SelectionSet selection'}) value =
        resolveBySelectionM selection' (__typename : resolversBy value)
        where
          __typename = ("__typename", const $ return $ return $ Scalar $ String $typeID (Proxy @a))
      encodeObject (key, Selection {selectionPosition = position'}) _ =
        failResolveT $ subfieldsNotSelected key "" position'

instance (Monad m, EncodeUnionConstraint m a MResult) => Encoder m a UNION MResult where
  __encode _ = encodeUnion
    where
      encodeUnion :: Encode_ m a MResult
      encodeUnion (key', sel@Selection {selectionRec = UnionSelection selections'}) value =
        resolver (key', sel {selectionRec = SelectionSet (lookupSelectionByType type' selections')})
        where
          (type', resolver) = currentResolver (from value)
      encodeUnion _ _ = internalErrorT "union Resolver only should recieve UnionSelection"

instance (Monad m, Encoder m a (KIND a) MResult, Args.GQLArgs p) => Encoder m (Resolver m MUTATION p a) WRAPPER MResult where
  __encode _ selection'@(key', Selection {selectionArguments = astArgs', selectionPosition = position'}) (Resolver resolver) = do
    args <- ExceptT $ pure $ Args.decode astArgs'
    WithEffect effects1 value1 <- liftResolver position' key' (resolver args)
    WithEffect effects2 value2 <- _encode selection' value1
    return $ WithEffect (effects1 ++ effects2) value2

instance (Monad m, Encoder m a (KIND a) MResult, Args.GQLArgs p) => Encoder m (Resolver m QUERY p a) WRAPPER MResult where
  __encode _ selection'@(key', Selection {selectionArguments = astArgs', selectionPosition = position'}) (Resolver resolver) = do
    args <- ExceptT $ pure $ Args.decode astArgs'
    liftResolver position' key' (resolver args) >>= _encode selection'

instance (Monad m, Encoder m a (KIND a) MResult) => Encoder m (Maybe a) WRAPPER MResult where
  __encode _ = encodeMaybe (return Null) _encode

instance (Monad m, Encoder m a (KIND a) MResult) => Encoder m [a] WRAPPER MResult where
  __encode _ = encodeListM _encode
    where
      encodeListM :: Encode_ m a (WithEffect Value) -> Encode_ m [a] (WithEffect Value)
      encodeListM _ (_, Selection {selectionRec = SelectionField {}}) _ = pure $ pure (List [])
      encodeListM f query list = do
        value' <- mapM (f query) list
        return $ WithEffect (concatMap resultEffects value') (List (map resultValue value'))

-- GENERIC Instances
instance Encoder m a (KIND a) res => DeriveResolvers m (K1 s a) res where
  deriveResolvers key' (K1 src) = [(key', (`_encode` src))]

instance (GQLType a, Encoder m a (KIND a) res) => UnionResolvers m (K1 s a) res where
  currentResolver (K1 src) = (typeID (Proxy @a), (`_encode` src))

liftResolver :: Monad m => Position -> Text -> m (Either String a) -> ResolveT m a
liftResolver position' typeName' x = do
  result <- lift x
  case result of
    Left message' -> failResolveT $ fieldNotResolved position' typeName' (pack message')
    Right value   -> pure value

encodeMaybe :: Monad m => res -> Encode_ m a res -> Encode_ m (Maybe a) res
encodeMaybe defaultValue _ _ Nothing = pure defaultValue
encodeMaybe _ f query (Just value)   = f query value
