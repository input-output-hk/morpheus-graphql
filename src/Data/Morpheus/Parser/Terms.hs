{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Terms
  ( onType
  , spreadLiteral
  , nonNull
  , parseChar
  , parseMaybeTuple
  , parseTuple
  , parseAssignment
  , parseWhenChar
  , lookAheadChar
  ) where

import           Data.Functor                      (void, ($>))
import           Data.Morpheus.Parser.Internal     (Parser, Position)
import           Data.Morpheus.Parser.Primitive    (token)
import           Data.Morpheus.Types.Internal.Data (DataTypeWrapper (..))
import           Data.Text                         (Text)
import           Text.Megaparsec                   (anySingle, getSourcePos, label, lookAhead, sepBy, try, (<?>), (<|>))
import           Text.Megaparsec.Char              (char, space, space1, string)

nonNull :: Parser [DataTypeWrapper]
nonNull = (char '!' $> [NonNullType]) <|> pure []

lookAheadChar :: Parser Char
lookAheadChar = lookAhead (space >> anySingle)

parseWhenChar :: Char -> Parser a -> Parser a -> Parser a
parseWhenChar char' parser1 parser2 = label ("parseWhenChar(" <> [char'] <> ")") $ do
  x <- lookAheadChar
  if x == char'
    then parser1
    else parser2

parseMaybeTuple :: Parser a -> Parser [a]
parseMaybeTuple parser = parseTuple parser <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser = label "Tuple" $ do
  parseChar '('
  space
  values <- parser `sepBy` try (space *> char ',' *> space) <?> "empty Tuple value!"
  space
  parseChar ')'
  return values

parseAssignment :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, b)
parseAssignment nameParser' valueParser' = label "assignment" $ do
  name' <- nameParser'
  space
  void $ char ':'
  space
  value' <- valueParser'
  pure (name', value')

parseChar :: Char -> Parser ()
parseChar = void . char

onType :: Parser Text
onType = do
  _ <- string "on"
  space1
  token

spreadLiteral :: Parser Position
spreadLiteral = do
  index <- getSourcePos
  _ <- string "..."
  return index
