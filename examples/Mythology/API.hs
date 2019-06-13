{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

module Mythology.API
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         ()
import           Data.Morpheus.Types        ((::->), GQLArgs, GQLRootResolver (..), Resolver (..), Resolver, QUERY)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Mythology.Character.Deity  (Deity (..), dbDeity)

newtype Query m = Query
  { deity :: Resolver m QUERY DeityArgs Deity
  } deriving (Generic)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic, GQLArgs)

resolveDeity :: DeityArgs ::-> Deity
resolveDeity = Resolver $ \args -> dbDeity (name args) (mythology args)

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi =
  interpreter
    GQLRootResolver {queryResolver = Query {deity = resolveDeity}, mutationResolver = (), subscriptionResolver = ()}
