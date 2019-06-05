{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
  ( body
  , entries
  ) where

import           Control.Applicative                           ((<|>))
import           Data.Char                                     (isAlpha)
import           Data.Functor                                  (void)
import           Data.Morpheus.Parser.Arguments                (maybeArguments)
import           Data.Morpheus.Parser.Internal                 (Parser)
import           Data.Morpheus.Parser.Primitive                (qualifier, token)
import           Data.Morpheus.Parser.Terms                    (lookAheadChar, onType, parseAssignment, parseChar,
                                                                parseWhenChar, spreadLiteral)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), RawArguments, RawSelection (..),
                                                                RawSelection' (..), RawSelectionSet, Reference (..))
import           Data.Text                                     (pack, Text)
import           System.IO                                     (hPutStr)
import           Text.Megaparsec                               (try, sepEndBy, getSourcePos, label, lookAhead, many, oneOf, sepBy)
import           Text.Megaparsec.Char                          (char, letterChar, space)
import           Text.Megaparsec.Debug                         (dbg)

spread :: Parser (Text, RawSelection)
spread = dbg "spread" $ label "spread" $ do
  index <- spreadLiteral
  space
  key' <- token
  return (key', Spread $ Reference {referenceName = key', referencePosition = index})

inlineFragment :: Parser (Text, RawSelection)
inlineFragment = dbg "InlineFragment" $ label "InlineFragment" $ do
  index <- spreadLiteral
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
parseSelectionField = dbg "SelectionField" $ label "SelectionField" $ do
  (name', position') <- qualifier
  arguments' <- maybeArguments
  value' <- parseWhenChar '{' (body arguments') (buildField arguments' position')
  return (name', value')
  where
    buildField arguments' position' =
      pure
        (RawSelectionField $
         RawSelection' {rawSelectionArguments = arguments', rawSelectionRec = (), rawSelectionPosition = position'})

alias :: Parser (Text, RawSelection)
alias = dbg "alias" $ label "alias" $ do
  ((name', position'), selection') <- parseAssignment qualifier parseSelectionField
  return (name', RawAlias {rawAliasPosition = position', rawAliasSelection = selection'})

bodySeparator :: Parser Char
bodySeparator = label "bodySeparator" $ oneOf [',', ' ', '\n', '\t']

entries :: Parser RawSelectionSet
entries = dbg "entries" $ label "entries" $ do
  parseChar '{'
  space
  entries' <- entry `sepBy` bodySeparator
  space
  parseChar '}'
  return entries'
  where
    entry = dbg "entry" $ label "entry" $ do
      (try (lookAhead (char '.')) *> (try inlineFragment <|> spread))
      <|>
      (try (lookAhead (letterChar <|> char '_')) *> (try alias <|> try parseSelectionField))

    -- TODO Learn about `try` and `<|>` - it doesn't have the semantics you assume it does.
body :: RawArguments -> Parser RawSelection
body args = label "body" $ do
  space
  index <- getSourcePos
  entries' <- entries
  return
    (RawSelectionSet $
     RawSelection' {rawSelectionArguments = args, rawSelectionRec = entries', rawSelectionPosition = index})
