{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Resolve.Generics.UnionResolvers
  ( UnionResolvers(..)
  , lookupSelectionByType
  ) where

import           Data.Maybe                                 (fromMaybe)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection, SelectionSet)
import           Data.Morpheus.Types.Internal.Validation    (ResolveT)
import           Data.Text                                  (Text)
import           GHC.Generics

-- SPEC: if there is no any fragment that supports current object Type GQL returns {}
lookupSelectionByType :: Text -> [(Text, SelectionSet)] -> SelectionSet
lookupSelectionByType type' sel = fromMaybe [] $ lookup type' sel

class UnionResolvers m f res where
  currentResolver :: f a -> (Text, (Text, Selection) -> ResolveT m res)

instance UnionResolvers m f res => UnionResolvers m (M1 S s f) res where
  currentResolver (M1 x) = currentResolver x

instance UnionResolvers m f res => UnionResolvers m (M1 D c f) res where
  currentResolver (M1 x) = currentResolver x

instance UnionResolvers m f res => UnionResolvers m (M1 C c f) res where
  currentResolver (M1 x) = currentResolver x

instance (UnionResolvers m a res, UnionResolvers m b res) => UnionResolvers m (a :+: b) res where
  currentResolver (L1 x) = currentResolver x
  currentResolver (R1 x) = currentResolver x
