{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
  ( body
  , entries
  ) where

import           Data.Morpheus.Parser.Arguments                (maybeArguments)
import           Data.Morpheus.Parser.Internal                 (Parser)
import           Data.Morpheus.Parser.Primitive                (qualifier, token)
import           Data.Morpheus.Parser.Terms                    (onType, parseAssignment, parseChar, parseWhenChar,
                                                                spreadLiteral)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), RawArguments, RawSelection (..),
                                                                RawSelection' (..), RawSelectionSet, Reference (..))
import           Data.Text                                     (Text)
import           Text.Megaparsec                               (getSourcePos, label, many, manyTill, oneOf, sepEndBy,
                                                                try, (<|>))
import           Text.Megaparsec.Char                          (space)
import           Text.Megaparsec.Debug                         (dbg)

spread :: Parser (Text, RawSelection)
spread = label "spread" $ do
  index <- spreadLiteral
  space
  key' <- token
  return (key', Spread $ Reference {referenceName = key', referencePosition = index})

inlineFragment :: Parser (Text, RawSelection)
inlineFragment = label "InlineFragment" $ do
  index <- spreadLiteral
  space
  type' <- onType
  space
  fragmentBody <- entries
  pure
    ( "INLINE_FRAGMENT"
    , InlineFragment $ Fragment {fragmentType = type', fragmentSelection = fragmentBody, fragmentPosition = index})

{-
  accept:
  - field
  - field {...}
  - field (...)
  - field () {...}
-}
parseSelectionField :: Parser (Text, RawSelection)
parseSelectionField = label "SelectionField" $ do
  (name', position') <- qualifier
  space
  arguments' <- maybeArguments
  space
  value' <- parseWhenChar '{' (space *> body arguments') (buildField arguments' position')
  space
  return (name', value')
  where
    buildField arguments' position' =
      pure
        (RawSelectionField $
         RawSelection' {rawSelectionArguments = arguments', rawSelectionRec = (), rawSelectionPosition = position'})

alias :: Parser (Text, RawSelection)
alias = label "alias" $ do
  ((name', position'), selection') <- parseAssignment qualifier parseSelectionField
  return (name', RawAlias {rawAliasPosition = position', rawAliasSelection = selection'})

bodySeparator :: Parser [Char]
bodySeparator = label "bodySeparator" $ many $ oneOf [',', ' ', '\n', '\t']

entries :: Parser RawSelectionSet
entries = label "entries" $ do
  parseChar '{'
  space
  entries' <- manyTill entry (parseChar '}')
  space
  return entries'
  where
    entry = dbg "entry" $ label "entry" $ do
      -- entry <- inlineFragment <|> spread <|> alias <|> parseSelectionField
      entry <- parseSelectionField
      space
      return entry


body :: RawArguments -> Parser RawSelection
body args = label "body" $ do
  index <- getSourcePos
  entries' <- entries
  return
    (RawSelectionSet $
     RawSelection' {rawSelectionArguments = args, rawSelectionRec = entries', rawSelectionPosition = index})
