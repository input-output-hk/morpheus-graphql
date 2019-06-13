{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Resolve.Generics.DeriveResolvers
  ( DeriveResolvers(..)
  , resolversBy
  , resolveBySelection
  , resolveBySelectionM
  ) where

import           Data.Maybe                                 (fromMaybe)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Validation    (ResolveT)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.Resolver               (WithEffect (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

-- type D1 = M1 D
-- type C1 = M1 C
-- type S1 = M1 S
-- M1 : Meta-information (constructor names, etc.)
-- D  :Datatype : Class for dataTypes that represent dataTypes
-- C :Constructor :
-- S - Selector: Class for dataTypes that represent records
-- Rep = D1 (...)  (C1 ...) (S1 (...) :+: D1 (...)  (C1 ...) (S1 (...)
unwrapMonadTuple :: Monad m => (Text, m a) -> m (Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

type ContextRes = WithEffect Value

type QueryRes = Value

type SelectRes m a = [(Text, (Text, Selection) -> ResolveT m a)] -> (Text, Selection) -> ResolveT m (Text, a)

type ResolveSel m a = [(Text, Selection)] -> [(Text, (Text, Selection) -> ResolveT m a)] -> ResolveT m a

selectResolver :: Monad m => a -> SelectRes m a
selectResolver defaultValue resolvers' (key', selection') =
  case selectionRec selection' of
    SelectionAlias name' aliasSelection' ->
      unwrapMonadTuple (key', lookupResolver name' (selection' {selectionRec = aliasSelection'}))
    _ -> unwrapMonadTuple (key', lookupResolver key' selection')
  where
    lookupResolver resolverKey' sel =
      (fromMaybe (const $ pure $defaultValue) $ lookup resolverKey' resolvers') (key', sel)

resolveBySelection :: Monad m => ResolveSel m QueryRes
resolveBySelection selection resolvers = Object <$> traverse (selectResolver Null resolvers) selection

resolveBySelectionM :: Monad m => ResolveSel m ContextRes
resolveBySelectionM selection resolvers = do
  value <- traverse (selectResolver (pure Null) resolvers) selection
  let value' = fmap (\(x, v) -> (x, resultValue v)) value
  let effects = concatMap (resultEffects . snd) value
  pure $ WithEffect effects (Object value')

resolversBy :: (Generic a, DeriveResolvers m (Rep a) res) => a -> [(Text, (Text, Selection) -> ResolveT m res)]
resolversBy = deriveResolvers "" . from

class DeriveResolvers m f res where
  deriveResolvers :: Text -> f a -> [(Text, (Text, Selection) -> ResolveT m res)]

instance DeriveResolvers m U1 res where
  deriveResolvers _ _ = []

instance (Selector s, DeriveResolvers m f res) => DeriveResolvers m (M1 S s f) res where
  deriveResolvers _ m@(M1 src) = deriveResolvers (pack $ selName m) src

instance DeriveResolvers m f res => DeriveResolvers m (M1 D c f) res where
  deriveResolvers key' (M1 src) = deriveResolvers key' src

instance DeriveResolvers m f res => DeriveResolvers m (M1 C c f) res where
  deriveResolvers key' (M1 src) = deriveResolvers key' src

instance (DeriveResolvers m f res, DeriveResolvers m g res) => DeriveResolvers m (f :*: g) res where
  deriveResolvers meta (a :*: b) = deriveResolvers meta a ++ deriveResolvers meta b
