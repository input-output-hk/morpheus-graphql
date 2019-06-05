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

import           Control.Applicative               ((<|>))
import           Data.Functor                      (void, ($>))
import           Data.Maybe                        (fromMaybe)
import           Data.Morpheus.Parser.Internal     (Parser, Position)
import           Data.Morpheus.Parser.Primitive    (token)
import           Data.Morpheus.Types.Internal.Data (DataTypeWrapper (..))
import           Data.Text                         (Text)
import           Text.Megaparsec                   (anySingle, getSourcePos, label, lookAhead, optional, sepBy, try,
                                                    (<?>))
import           Text.Megaparsec.Char              (char, space, string)
import           Text.Megaparsec.Debug             (dbg)

nonNull :: Parser [DataTypeWrapper]
nonNull = (char '!' $> [NonNullType]) <|> pure []

lookAheadChar :: Parser Char
lookAheadChar = lookAhead (space >> anySingle)

parseWhenChar :: Show a => Char -> Parser a -> Parser a -> Parser a
parseWhenChar char' parser1 parser2 = label ("parseWhenChar(" <> [char'] <> ")") $ do
  x <- lookAheadChar
  if x == char'
    then parser1
    else parser2

parseMaybeTuple :: Show a => Parser a -> Parser [a]
parseMaybeTuple parser = fromMaybe [] <$> optional (parseTuple parser)

parseTuple :: Show a => Parser a -> Parser [a]
parseTuple parser = label "Tuple" $ do
  space
  parseChar '('
  space
  values <- parser `sepBy` (space *> char ',') <?> "empty Tuple value!"
  space
  parseChar ')'
  return values

parseAssignment :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, b)
parseAssignment nameParser' valueParser' = dbg "assignment" $ label "assignment" $ do
  space
  name' <- nameParser'
  space
  void $ char ':'
  space
  value' <- valueParser'
  pure (name', value')

parseChar :: Char -> Parser ()
parseChar char' = do
  x <- anySingle
  if x == char'
    then return ()
    else fail ("expected '" ++ [char'] ++ "' found '" ++ [x] ++ "'")

onType :: Parser Text
onType = do
  space
  _ <- string "on "
  space
  token

spreadLiteral :: Parser Position
spreadLiteral = do
  space
  index <- getSourcePos
  _ <- string "..."
  return index
