{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.Holistic.API
  -- ( api
  -- ) where
  where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (ENUM, INPUT_OBJECT, KIND, OBJECT, SCALAR)
import           Data.Morpheus.Types        (WithEffect, MUTATION, Resolver(Resolver), QUERY, (::->), GQLArgs, GQLRootResolver (..), GQLScalar (..), GQLType (..),
                                             ScalarValue (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

type instance KIND CityID = ENUM

type instance KIND Euro = SCALAR

type instance KIND UID = INPUT_OBJECT

type instance KIND Coordinates = INPUT_OBJECT

type instance KIND Address = OBJECT

type instance KIND User = OBJECT

data CityID
  = Paris
  | BLN
  | HH
  deriving (Generic, GQLType)

data Euro =
  Euro Int
       Int
  deriving (Generic, GQLType)

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

newtype UID = UID
  { uid :: Text
  } deriving (Generic, GQLType)

data Coordinates = Coordinates
  { latitude  :: Euro
  , longitude :: [UID]
  } deriving (Generic, GQLType)

data Address = Address
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  , owner       :: Maybe User
  } deriving (Generic, GQLType)

data AddressArgs = AddressArgs
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic, GQLArgs)

data OfficeArgs = OfficeArgs
  { zipCode :: Maybe [Int]
  , cityID  :: CityID
  } deriving (Generic, GQLArgs)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: AddressArgs ::-> Address
  , office  :: OfficeArgs ::-> Address
  , friend  :: () ::-> Maybe User
  , home    :: CityID
  } deriving (Generic)

instance GQLType User where
  description _ = "Custom Description for Client Defined User Type"

newtype Query m = Query
  { user :: Resolver m QUERY () User
  } deriving (Generic)

newtype Mutation m = Mutation
  { createUser :: Resolver m MUTATION AddressArgs User
  } deriving (Generic)

newtype Subscription m = Subscription
  { newUser :: Resolver m MUTATION AddressArgs User
  } deriving (Generic)

resolveAddress :: Monad m => Resolver m QUERY a Address
resolveAddress = return Address {city = "", houseNumber = 1, street = "", owner = Nothing}

testUser :: User
testUser =
  User
      { name = "testName"
      , email = ""
      , address = resolveAddress
      , office = resolveAddress
      , home = HH
      , friend = return Nothing
      }

resolveUser :: Monad m => Resolver m QUERY () User
resolveUser = Resolver $ \_ -> return $ return testUser

createUserMutation :: Monad m => Resolver m MUTATION AddressArgs User
createUserMutation = Resolver $ \args -> return $ return $ return testUser

newUserSubscription :: Monad m => Resolver m MUTATION AddressArgs User
newUserSubscription = Resolver $ \args -> return $ return $ return testUser

api :: Monad m => ByteString -> m ByteString
api =
  interpreter
    GQLRootResolver
      { queryResolver = Query {user = resolveUser}
      , mutationResolver = Mutation {createUser = createUserMutation}
      , subscriptionResolver = Subscription {newUser = newUserSubscription}
      }
