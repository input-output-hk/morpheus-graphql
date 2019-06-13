{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Types.GQLOperator where

import           Data.Morpheus.Resolve.Encode                   (_encode)
import           Data.Morpheus.Resolve.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection,
                                                                 resolveBySelectionM)
import           Data.Morpheus.Resolve.Generics.ObjectRep       (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Resolve.Introspect               (_introspect)
import           Data.Morpheus.Schema.Schema                    (Schema, initSchema)
import           Data.Morpheus.Types.Internal.AST.Selection     (SelectionSet)
import           Data.Morpheus.Types.Internal.Data              (DataOutputField, DataType (..), DataTypeLib (..),
                                                                 initTypeLib)
import           Data.Morpheus.Types.Internal.Validation        (ResolveT)
import           Data.Morpheus.Types.Internal.Value             (Value (..))
import           Data.Morpheus.Types.Resolver                   (WithEffect (..))
import           Data.Proxy
import           Data.Text                                      (Text)
import           GHC.Generics

type QResult = Value

type MResult = WithEffect Value

type Encode m a r = a -> SelectionSet -> ResolveT m r

type EncodeCon m a r = (Monad m, Generic a, DeriveResolvers m (Rep a) r)

type IntroCon a = (ObjectRep (Rep a) (Text, DataOutputField))

operatorType :: Text -> a -> (Text, DataType a)
operatorType name' fields' = (name', DataType {typeData = fields', typeName = name', typeDescription = ""})

encodeQuery :: forall m a. EncodeCon m a QResult => DataTypeLib -> Encode m a QResult
encodeQuery types rootResolver sel = resolveBySelection sel (schemaResolver ++ resolvers)
  where
    schemaResolver = [("__schema", (`_encode` (initSchema types :: Schema m)))]
    resolvers = deriveResolvers "" $ from rootResolver

querySchema :: forall a. IntroCon a => a -> DataTypeLib
querySchema _ = resolveTypes typeLib stack'
  where
    typeLib = _introspect (Proxy :: Proxy (Schema m)) queryType
    queryType = initTypeLib (operatorType "Query" fields')
    (fields', stack') = unzip $ getFields (Proxy @(Rep a))

encodeMutation :: EncodeCon m a MResult => Encode m a MResult
encodeMutation rootResolver sel = resolveBySelectionM sel $ deriveResolvers "" $ from rootResolver

mutationSchema :: forall a. IntroCon a => a -> DataTypeLib -> DataTypeLib
mutationSchema _ initialType = resolveTypes mutationType types'
  where
    mutationType = initialType {mutation = Just $ operatorType "Mutation" fields'}
    (fields', types') = unzip $ getFields (Proxy :: Proxy (Rep a))

encodeSubscription :: EncodeCon m a MResult => Encode m a MResult
encodeSubscription rootResolver sel = resolveBySelectionM sel $ deriveResolvers "" $ from rootResolver

subscriptionSchema :: forall a. IntroCon a => a -> DataTypeLib -> DataTypeLib
subscriptionSchema _ initialType = resolveTypes subscriptionType types'
  where
    subscriptionType = initialType {subscription = Just $ operatorType "Subscription" fields'}
    (fields', types') = unzip $ getFields (Proxy :: Proxy (Rep a))
