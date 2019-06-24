{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Resolve.Generics.DeriveResolvers
  ( ObjectFieldResolvers(..)
  , UnionResolvers(..)
  , resolversBy
  , resolveBySelection
  , resolveBySelectionM
  , lookupSelectionByType
  ) where

import           Data.Maybe                                 (fromMaybe)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Validation    (ResolveT)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.Resolver               (WithEffect (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

type ContextRes = WithEffect Value

type QueryRes = Value

type SelectRes m a = [(Text, (Text, Selection) -> ResolveT m a)] -> (Text, Selection) -> ResolveT m (Text, a)

type ResolveSel m a = [(Text, Selection)] -> [(Text, (Text, Selection) -> ResolveT m a)] -> ResolveT m a

--
-- OBJECT
--
class ObjectFieldResolvers m f res where
  objectFieldResolvers :: Text -> f a -> [(Text, (Text, Selection) -> ResolveT m res)]

instance ObjectFieldResolvers m U1 res where
  objectFieldResolvers _ _ = []

instance (Selector s, ObjectFieldResolvers m f res) => ObjectFieldResolvers m (M1 S s f) res where
  objectFieldResolvers _ m@(M1 src) = objectFieldResolvers (pack $ selName m) src

instance ObjectFieldResolvers m f res => ObjectFieldResolvers m (M1 D c f) res where
  objectFieldResolvers key' (M1 src) = objectFieldResolvers key' src

instance ObjectFieldResolvers m f res => ObjectFieldResolvers m (M1 C c f) res where
  objectFieldResolvers key' (M1 src) = objectFieldResolvers key' src

instance (ObjectFieldResolvers m f res, ObjectFieldResolvers m g res) => ObjectFieldResolvers m (f :*: g) res where
  objectFieldResolvers meta (a :*: b) = objectFieldResolvers meta a ++ objectFieldResolvers meta b

unwrapMonadTuple :: Monad m => (Text, m a) -> m (Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

selectResolver :: Monad m => a -> SelectRes m a
selectResolver defaultValue resolvers' (key', selection') =
  case selectionRec selection' of
    SelectionAlias name' aliasSelection' ->
      unwrapMonadTuple (key', lookupResolver name' (selection' {selectionRec = aliasSelection'}))
    _ -> unwrapMonadTuple (key', lookupResolver key' selection')
  where
    lookupResolver resolverKey' sel =
      (fromMaybe (const $ return $defaultValue) $ lookup resolverKey' resolvers') (key', sel)

resolveBySelection :: Monad m => ResolveSel m QueryRes
resolveBySelection selection resolvers = Object <$> mapM (selectResolver Null resolvers) selection

resolveBySelectionM :: Monad m => ResolveSel m ContextRes
resolveBySelectionM selection resolvers = do
  value <- mapM (selectResolver (return Null) resolvers) selection
  let value' = fmap (\(x, v) -> (x, resultValue v)) value
  let effects = concatMap (resultEffects . snd) value
  return $ WithEffect effects (Object value')

resolversBy :: (Generic a, ObjectFieldResolvers m (Rep a) res) => a -> [(Text, (Text, Selection) -> ResolveT m res)]
resolversBy = objectFieldResolvers "" . from

--
-- UNION
--
-- SPEC: if there is no any fragment that supports current object Type GQL returns {}
lookupSelectionByType :: Text -> [(Text, SelectionSet)] -> SelectionSet
lookupSelectionByType type' sel = fromMaybe [] $ lookup type' sel

class UnionResolvers m f res where
  unionResolvers :: f a -> (Text, (Text, Selection) -> ResolveT m res)

instance UnionResolvers m f res => UnionResolvers m (M1 S s f) res where
  unionResolvers (M1 x) = unionResolvers x

instance UnionResolvers m f res => UnionResolvers m (M1 D c f) res where
  unionResolvers (M1 x) = unionResolvers x

instance UnionResolvers m f res => UnionResolvers m (M1 C c f) res where
  unionResolvers (M1 x) = unionResolvers x

instance (UnionResolvers m a res, UnionResolvers m b res) => UnionResolvers m (a :+: b) res where
  unionResolvers (L1 x) = unionResolvers x
  unionResolvers (R1 x) = unionResolvers x
