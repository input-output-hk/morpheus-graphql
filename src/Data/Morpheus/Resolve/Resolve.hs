{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Resolve.Resolve
  ( resolve
  , resolveByteString
  , resolveStreamByteString
  , resolveStream
  , packStream
  ) where

import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT)
import           Data.Aeson                                 (eitherDecode, encode)
import           Data.ByteString.Lazy.Char8                 (ByteString)
import           Data.Morpheus.Error.Utils                  (badRequestError, renderErrors)
import           Data.Morpheus.Parser.Parser                (parseGQL)
import           Data.Morpheus.Server.ClientRegister        (GQLState, publishUpdates)
import           Data.Morpheus.Types.GQLOperator            (GQLMutation (..), GQLQuery (..), GQLSubscription (..))
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (SchemaValidation)
import           Data.Morpheus.Types.Internal.WebSocket     (OutputAction (..))
import           Data.Morpheus.Types.Request                (GQLRequest (..))
import           Data.Morpheus.Types.Resolver               (WithEffect (..))
import           Data.Morpheus.Types.Response               (GQLResponse (..))
import           Data.Morpheus.Types.Types                  (GQLRootResolver (..))
import           Data.Morpheus.Validation.Validation        (validateRequest)
import           Data.Proxy                                 (Proxy (Proxy))

schema :: forall m a b c. (Monad m, GQLQuery m a, GQLMutation m b, GQLSubscription m c) => GQLRootResolver m a b c -> SchemaValidation DataTypeLib
schema GQLRootResolver {..} =
  querySchema (Proxy :: Proxy (m a))
  >>= mutationSchema (Proxy :: Proxy (m b))
  >>= subscriptionSchema (Proxy :: Proxy (m c))

resolveByteString ::
     (Monad m, GQLQuery m a, GQLMutation m b, GQLSubscription m c) => GQLRootResolver m a b c -> ByteString -> m ByteString
resolveByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolve rootResolver req

resolveStreamByteString ::
     (Monad m, GQLQuery m a, GQLMutation m b, GQLSubscription m c)
  => GQLRootResolver m a b c
  -> ByteString
  -> m (OutputAction m ByteString)
resolveStreamByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ NoEffect $ badRequestError aesonError'
    Right req        -> fmap encode <$> resolveStream rootResolver req

resolve :: forall m a b c. (Monad m, GQLQuery m a, GQLMutation m b, GQLSubscription m c) => GQLRootResolver m a b c -> GQLRequest -> m GQLResponse
resolve rootResolver@GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver} request =
  case schema rootResolver of
    Left error' -> return $ Errors $ renderErrors error'
    Right validSchema -> do
      value <- runExceptT $ _resolve validSchema
      case value of
        Left x  -> return $ Errors $ renderErrors x
        Right x -> return $ Data x
      where _resolve gqlSchema = do
              rootGQL <- ExceptT $ pure (parseGQL request >>= validateRequest gqlSchema)
              case rootGQL of
                Query operator' -> encodeQuery gqlSchema queryResolver $ operatorSelection operator'
                Mutation operator' -> resultValue <$> encodeMutation mutationResolver (operatorSelection operator')
                Subscription operator' ->
                  resultValue <$> encodeSubscription subscriptionResolver (operatorSelection operator')

resolveStream ::
  forall m a b c.
  (Monad m, GQLQuery m a, GQLMutation m b, GQLSubscription m c)
  => GQLRootResolver m a b c
  -> GQLRequest
  -> m (OutputAction m GQLResponse)
resolveStream rootResolver@GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver} request =
  case schema rootResolver of
    Left error' -> return $ NoEffect $ Errors $ renderErrors error'
    Right validSchema -> do
      value <- runExceptT $ _resolve validSchema
      case value of
        Left x       -> return $ NoEffect $ Errors $ renderErrors x
        Right value' -> return $ fmap Data value'
  where
    _resolve gqlSchema = (ExceptT $ pure (parseGQL request >>= validateRequest gqlSchema)) >>= resolveOperator
      where
        resolveOperator (Query operator') = do
          value <- encodeQuery gqlSchema queryResolver $ operatorSelection operator'
          return (NoEffect value)
        resolveOperator (Mutation operator') = do
          WithEffect mutationChannels mutationResponse <- encodeMutation mutationResolver $ operatorSelection operator'
          return PublishMutation {mutationChannels, mutationResponse, currentSubscriptionStateResolver}
          where
            currentSubscriptionStateResolver :: SelectionSet -> m GQLResponse
            currentSubscriptionStateResolver selection' = do
              value <- runExceptT (encodeSubscription subscriptionResolver selection')
              case value of
                Left x                  -> pure $ Errors $ renderErrors x
                Right (WithEffect _ x') -> pure $ Data x'
        resolveOperator (Subscription operator') = do
          WithEffect subscriptionChannels _ <- encodeSubscription subscriptionResolver $ operatorSelection operator'
          return InitSubscription {subscriptionChannels, subscriptionQuery = operatorSelection operator'}

packStream :: GQLState -> (ByteString -> IO (OutputAction IO ByteString)) -> ByteString -> IO ByteString
packStream state streamAPI request = do
  value <- streamAPI request
  case value of
    PublishMutation {mutationChannels, mutationResponse, currentSubscriptionStateResolver} -> do
      publishUpdates mutationChannels currentSubscriptionStateResolver state
      return mutationResponse
    InitSubscription {} -> pure "subscriptions are only allowed in websocket"
    NoEffect res' -> return res'
