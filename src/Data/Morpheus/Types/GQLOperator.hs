{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Types.GQLOperator
  ( GQLQuery(..)
  , GQLMutation(..)
  , GQLSubscription(..)
  ) where

import           Data.Morpheus.Resolve.Encode               (ObjectFieldResolvers (..), resolveBySelection,
                                                             resolveBySelectionM, resolversBy)
import           Data.Morpheus.Resolve.Generics.TypeRep     (ObjectRep (..), TypeUpdater, resolveTypes)
import           Data.Morpheus.Resolve.Introspect           (introspectOutputType)
import           Data.Morpheus.Schema.Schema                (Schema, Type, findType, initSchema)
import           Data.Morpheus.Types.GQLArgs                (GQLArgs)
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataArguments, DataField (..), DataType (..),
                                                             DataTypeLib (..), initTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (ResolveT, SchemaValidation)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.Resolver               (QUERY, Resolver (..), WithEffect (..))
import           Data.Proxy
import           Data.Text                                  (Text)
import           Data.Typeable                              (Typeable, typeRep, typeRepFingerprint)
import           GHC.Generics

type QResult = Value

type MResult = WithEffect Value

type Encode m a r = a -> SelectionSet -> ResolveT m r

type EncodeCon m a r = (Monad m, Generic a, ObjectFieldResolvers m (Rep a) r)

type IntroCon a = (ObjectRep (Rep a) DataArguments, Typeable a)

operatorType :: Typeable t => Proxy t -> Text -> a -> (Text, DataType a)
operatorType proxy name' fields' =
  ( name'
  , DataType
      {typeData = fields', typeName = name', typeFingerprint = typeRepFingerprint $ typeRep proxy, typeDescription = ""})

newtype TypeArgs = TypeArgs
  { name :: Text
  } deriving (Generic, GQLArgs)

data SystemQuery m = SystemQuery
  { __type   :: Resolver m QUERY TypeArgs (Maybe (Type m))
  , __schema :: Schema m
  } deriving (Generic)

hideFields :: (Text, DataField a) -> (Text, DataField a)
hideFields (key', field) = (key', field {fieldHidden = True})

systemQuery :: Monad m => DataTypeLib -> SystemQuery m
systemQuery lib =
  SystemQuery {__type = Resolver $ \TypeArgs {name} -> return $ Right $ findType name lib, __schema = initSchema lib}

-- | derives GQL Query Operator
class GQLQuery m a where
  encodeQuery :: DataTypeLib -> Encode m a QResult
  default encodeQuery :: (Typeable m, EncodeCon m a QResult) => DataTypeLib -> Encode m a QResult
  encodeQuery types rootResolver sel = resolveBySelection sel (resolversBy (systemQuery types :: SystemQuery m) ++ resolversBy rootResolver)

  querySchema :: Proxy (m a) -> SchemaValidation DataTypeLib
  default querySchema :: (Typeable m, Monad m, IntroCon a) => Proxy (m a) -> SchemaValidation DataTypeLib
  querySchema _ = resolveTypes queryType (introspectOutputType (Proxy @(Schema m)) : stack')
    where
      queryType = initTypeLib (operatorType (Proxy @a) "Query" (__fields ++ fields'))
      __fields = map (hideFields . fst) $ objectFieldTypes $ Proxy @(Rep (SystemQuery m))
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

-- | derives GQL Subscription Mutation
class GQLMutation m a where
  encodeMutation :: Encode m a MResult
  default encodeMutation :: EncodeCon m a MResult => Encode m a MResult
  encodeMutation rootResolver sel = resolveBySelectionM sel $ resolversBy rootResolver

  mutationSchema :: Proxy (m a) -> TypeUpdater
  default mutationSchema :: IntroCon a => Proxy (m a) -> TypeUpdater
  mutationSchema _ initialType = resolveTypes mutationType types'
    where
      mutationType = initialType {mutation = Just $ operatorType (Proxy @a) "Mutation" fields'}
      (fields', types') = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

-- | derives GQL Subscription Operator
class GQLSubscription m a where
  encodeSubscription :: Encode m a MResult
  default encodeSubscription :: EncodeCon m a MResult => Encode m a MResult
  encodeSubscription rootResolver sel = resolveBySelectionM sel $ resolversBy rootResolver

  subscriptionSchema :: Proxy (m a) -> TypeUpdater
  default subscriptionSchema :: IntroCon a => Proxy (m a) -> TypeUpdater
  subscriptionSchema _ initialType = resolveTypes subscriptionType types'
    where
      subscriptionType = initialType {subscription = Just $ operatorType (Proxy @a) "Subscription" fields'}
      (fields', types') = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

instance (Monad m, Typeable m) => GQLQuery m ()

instance Monad m => GQLMutation m () where
  encodeMutation _ _ = pure $ pure Null
  mutationSchema _ = return

instance Monad m => GQLSubscription m () where
  encodeSubscription _ _ = pure $ pure Null
  subscriptionSchema _ = return
