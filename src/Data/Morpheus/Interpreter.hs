{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Morpheus.Interpreter
  ( Interpreter(..)
  ) where

import           Data.Aeson                             (encode)
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Lazy.Char8             as LB (ByteString, fromStrict, toStrict)
import           Data.Morpheus.Resolve.Resolve          (ResolveCon, packStream, resolve, resolveByteString, resolveStream,
                                                         resolveStreamByteString)
import           Data.Morpheus.Server.ClientRegister    (GQLState)
import           Data.Morpheus.Types.Internal.WebSocket (OutputAction)
import           Data.Morpheus.Types.Request            (GQLRequest)
import           Data.Morpheus.Types.Response           (GQLResponse)
import           Data.Morpheus.Types.Types              (GQLRootResolver (..))
import           Data.Text                              (Text)
import qualified Data.Text.Lazy                         as LT (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Encoding                (decodeUtf8, encodeUtf8)

class Interpreter f a b where
  interpreter :: ResolveCon f q m s => GQLRootResolver f q m s -> a -> f b

{-
  simple HTTP stateless Interpreter without side effects
-}
-- type StateLess m a = a -> m a

instance Interpreter m GQLRequest GQLResponse where
  interpreter = resolve

instance Interpreter m LB.ByteString LB.ByteString where
  interpreter = resolveByteString

instance Interpreter m LT.Text LT.Text where
  interpreter root request = decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter m ByteString ByteString where
  interpreter root request = LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter m Text Text where
  interpreter root request = LT.toStrict <$> interpreter root (LT.fromStrict request)

-- {-
--    HTTP Interpreter with state and side effects, every mutation will
--    trigger subscriptions in  shared `GQLState`
-- -}
-- type WSPub m a = (GQLState, a) -> m a

instance Interpreter IO (GQLState, LB.ByteString) LB.ByteString where
  interpreter root (state, request) = packStream state (resolveStreamByteString root) request

instance Interpreter IO (GQLState, LT.Text) LT.Text where
  interpreter root (state, request) = decodeUtf8 <$> interpreter root (state, encodeUtf8 request)

instance Interpreter IO (GQLState, ByteString) ByteString where
  interpreter root (state, request) = LB.toStrict <$> interpreter root (state, LB.fromStrict request)

instance Interpreter IO (GQLState, Text) Text where
  interpreter root (state, request) = LT.toStrict <$> interpreter root (state, LT.fromStrict request)

-- {-
--    Websocket Interpreter without state and side effects, mutations and subscription will return Actions
--    that will be executed in Websocket server
-- -}

instance Interpreter m GQLRequest (OutputAction m LB.ByteString) where
  interpreter root request = fmap encode <$> resolveStream root request

instance Interpreter m LB.ByteString (OutputAction m LB.ByteString) where
  interpreter = resolveStreamByteString

instance Interpreter m LT.Text (OutputAction m LT.Text) where
  interpreter root request = fmap decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter m ByteString (OutputAction m ByteString) where
  interpreter root request = fmap LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter m Text (OutputAction m Text) where
  interpreter root request = fmap LT.toStrict <$> interpreter root (LT.fromStrict request)
