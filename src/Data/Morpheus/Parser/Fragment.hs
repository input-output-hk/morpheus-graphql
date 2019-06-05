{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Fragment
  ( fragment
  ) where

import           Data.Morpheus.Parser.Body                     (entries)
import           Data.Morpheus.Parser.Internal                 (Parser)
import           Data.Morpheus.Parser.Primitive                (token)
import           Data.Morpheus.Parser.Terms                    (onType)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..))
import           Data.Text                                     (Text)
import           Text.Megaparsec                               (getSourcePos)
import           Text.Megaparsec.Char                          (space, string)

fragment :: Parser (Text, Fragment)
fragment = do
  space
  index <- getSourcePos
  _ <- string "fragment"
  space
  name' <- token
  type' <- onType
  space
  fragmentBody <- entries
  pure (name', Fragment {fragmentType = type', fragmentSelection = fragmentBody, fragmentPosition = index})
