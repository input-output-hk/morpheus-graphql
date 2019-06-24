{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Schema.Type
  ( Type(..)
  , DeprecationArgs(..)
  ) where

import           Data.Morpheus.Kind              (KIND, OBJECT)
import           Data.Morpheus.Schema.EnumValue  (EnumValue)
import qualified Data.Morpheus.Schema.Field      as F (Field (..))
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind)
import           Data.Morpheus.Types.Resolver    (QUERY, Resolver)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

type instance KIND (Type m) = OBJECT

data Type m = Type
  { kind          :: TypeKind
  , name          :: Maybe Text
  , description   :: Maybe Text
  , fields        :: Resolver m QUERY DeprecationArgs (Maybe [F.Field (Type m)])
  , interfaces    :: Maybe [Type m]
  , possibleTypes :: Maybe [Type m]
  , enumValues    :: Resolver m QUERY DeprecationArgs (Maybe [EnumValue])
  , inputFields   :: Maybe [I.InputValue (Type m)]
  , ofType        :: Maybe (Type m)
  } deriving (Generic)

newtype DeprecationArgs = DeprecationArgs
  { includeDeprecated :: Maybe Bool
  } deriving (Generic)
