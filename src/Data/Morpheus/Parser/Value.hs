{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Data.Morpheus.Parser.Value
  ( parseValue
  , enumValue
  ) where

import           Data.Functor                       (($>))
import           Data.Morpheus.Parser.Internal      (Parser)
import           Data.Morpheus.Parser.Primitive     (token)
import           Data.Morpheus.Parser.Terms         (parseAssignment)
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..), Value (..), decodeScientific)
import           Data.Text                          (pack)
import           Text.Megaparsec                    (anySingleBut, choice, label, many, sepBy, (<|>))
import           Text.Megaparsec.Char               (char, space, string)
import           Text.Megaparsec.Char.Lexer         (scientific)

parseValue :: Parser Value
parseValue = label "value" $ valueNull <|> booleanValue <|> valueNumber <|> stringValue <|> objectValue <|> listValue

valueNull :: Parser Value
valueNull = string "null" $> Null

booleanValue :: Parser Value
booleanValue = boolTrue <|> boolFalse
  where
    boolTrue = string "true" $> Scalar (Boolean True)
    boolFalse = string "false" $> Scalar (Boolean False)

valueNumber :: Parser Value
valueNumber = Scalar . decodeScientific <$> scientific

enumValue :: Parser Value
enumValue = Enum <$> token

escaped :: Parser Char
escaped = label "escaped" $ do
  x <- anySingleBut '\"'
  if x == '\\'
    then choice (zipWith escapeChar codes replacements)
    else pure x
  where
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']
    codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
    escapeChar code replacement = char code >> return replacement

stringValue :: Parser Value
stringValue = label "stringValue" $ do
  _ <- char '"'
  value <- many escaped
  _ <- char '"'
  pure $ Scalar $ String $ pack value

listValue :: Parser Value
listValue = label "listValue" $ do
  _ <- char '['
  space
  entries' <-
    (do space
        val <- parseValue
        space
        return val) `sepBy`
    char ','
  space
  _ <- char ']'
  return (List entries')

objectValue :: Parser Value
objectValue = label "objectValue" $ do
  _ <- char '{'
  space
  entries' <- parseAssignment token parseValue `sepBy` char ','
  space
  _ <- char '}'
  space
  return (Object entries')
