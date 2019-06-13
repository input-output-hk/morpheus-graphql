{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Resolve.Resolve
  ( resolve
  , resolveByteString
  , resolveStreamByteString
  , resolveStream
  , packStream
  , ResolveCon
  ) where

import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT)
import           Data.Aeson                                 (eitherDecode, encode)
import           Data.ByteString.Lazy.Char8                 (ByteString)
import           Data.Morpheus.Error.Utils                  (badRequestError, renderErrors)
import           Data.Morpheus.Parser.Parser                (parseGQL)
import           Data.Morpheus.Server.ClientRegister        (GQLState, publishUpdates)
import           Data.Morpheus.Types.GQLOperator            (EncodeCon, IntroCon, MResult, QResult, encodeMutation,
                                                             encodeQuery, encodeSubscription, mutationSchema,
                                                             querySchema, subscriptionSchema)
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataTypeLib)
import           Data.Morpheus.Types.Internal.WebSocket     (OutputAction (..))
import           Data.Morpheus.Types.Request                (GQLRequest (..))
import           Data.Morpheus.Types.Resolver               (WithEffect (..))
import           Data.Morpheus.Types.Response               (GQLResponse (..))
import           Data.Morpheus.Types.Types                  (GQLRootResolver (..))
import           Data.Morpheus.Validation.Validation        (validateRequest)
import           Data.String                                (IsString)

type ResolveCon f q m s
     = ( IntroCon q
       , IntroCon m
       , IntroCon s
       , EncodeCon f q QResult
       , EncodeCon f m MResult
       , EncodeCon f s MResult
       )

schema :: (IntroCon q, IntroCon m, IntroCon s) => GQLRootResolver f q m s -> DataTypeLib
schema GQLRootResolver {..} =
  subscriptionSchema subscriptionResolver $ mutationSchema mutationResolver $ querySchema queryResolver

resolveByteString :: ResolveCon f q m s => GQLRootResolver f q m s -> ByteString -> f ByteString
resolveByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolve rootResolver req

resolveStreamByteString ::
  ResolveCon f q m s
  => GQLRootResolver f q m s
  -> ByteString
  -> f (OutputAction f ByteString)
resolveStreamByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ NoEffect $ badRequestError aesonError'
    Right req        -> fmap encode <$> resolveStream rootResolver req

resolve :: ResolveCon f q m s => GQLRootResolver f q m s -> GQLRequest -> f GQLResponse
resolve rootResolver@GQLRootResolver { queryResolver = queryRes
                                     , mutationResolver = mutationRes
                                     , subscriptionResolver = subscriptionRes
                                     } request = do
  value <- runExceptT _resolve
  case value of
    Left x  -> pure $ Errors $ renderErrors x
    Right x -> pure $ Data x
  where
    _resolve = do
      rootGQL <- ExceptT $ pure (parseGQL request >>= validateRequest gqlSchema)
      case rootGQL of
        Query operator'        -> encodeQuery gqlSchema queryRes $ operatorSelection operator'
        Mutation operator'     -> resultValue <$> encodeMutation mutationRes (operatorSelection operator')
        Subscription operator' -> resultValue <$> encodeSubscription subscriptionRes (operatorSelection operator')
      where
        gqlSchema = schema rootResolver

resolveStream ::
  forall f q m s.
  ResolveCon f q m s
  => GQLRootResolver f q m s
  -> GQLRequest
  -> f (OutputAction f GQLResponse)
resolveStream  rootResolver@GQLRootResolver { queryResolver = queryRes
                                            , mutationResolver = mutationRes
                                            , subscriptionResolver = subscriptionRes
                                            } request = do
  value <- runExceptT _resolve
  case value of
    Left x       -> pure $ NoEffect $ Errors $ renderErrors x
    Right value' -> return $ fmap Data value'
  where
    _resolve = (ExceptT $ pure (parseGQL request >>= validateRequest gqlSchema)) >>= resolveOperator
      where
        resolveOperator (Query operator') = do
          value <- encodeQuery gqlSchema queryRes $ operatorSelection operator'
          return (NoEffect value)
        resolveOperator (Mutation operator') = do
          WithEffect channels value <- encodeMutation mutationRes $ operatorSelection operator'
          return
            PublishMutation
              {mutationChannels = channels, mutationResponse = value, currentSubscriptionStateResolver = sRes}
          where
            sRes :: SelectionSet -> f GQLResponse
            sRes selection' = do
              value <- runExceptT (encodeSubscription subscriptionRes selection')
              case value of
                Left x                  -> pure $ Errors $ renderErrors x
                Right (WithEffect _ x') -> pure $ Data x'
        resolveOperator (Subscription operator') = do
          WithEffect channels _ <- encodeSubscription subscriptionRes $ operatorSelection operator'
          return InitSubscription {subscriptionChannels = channels, subscriptionQuery = operatorSelection operator'}
        gqlSchema = schema rootResolver

packStream :: IsString a => GQLState -> (t -> IO (OutputAction IO a)) -> t -> IO a
packStream state streamAPI request = do
  value <- streamAPI request
  case value of
    PublishMutation {mutationChannels = channels, mutationResponse = res', currentSubscriptionStateResolver = resolver'} -> do
      publishUpdates channels resolver' state
      return res'
    InitSubscription {} -> pure "subscriptions are only allowed in websocket"
    NoEffect res' -> return res'
