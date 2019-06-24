{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Schema.Directive
  ( Directive(..)
  ) where

import           Data.Morpheus.Kind                                (KIND, OBJECT)
import           Data.Morpheus.Schema.DirectiveLocation            (DirectiveLocation)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (InputValue)
import           Data.Text                                         (Text)
import           GHC.Generics                                      (Generic)

type instance KIND (Directive m) = OBJECT

data Directive m = Directive
  { name        :: Text
  , description :: Maybe Text
  , locations   :: [DirectiveLocation]
  , args        :: [InputValue m]
  } deriving (Generic)
