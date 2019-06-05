{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Primitive where

import           Control.Applicative               (many, (<|>))
import           Data.Morpheus.Parser.Internal     (Parser)
import           Data.Morpheus.Types.Internal.Base (Position)
import           Data.Text                         (Text)
import qualified Data.Text                         as T (pack)
import           Text.Megaparsec                   (label, getSourcePos)
import           Text.Megaparsec.Char              (char, digitChar, letterChar, space)
import           Text.Megaparsec.Debug             (dbg)

replaceType :: Text -> Text
replaceType "type" = "_type"
replaceType x      = x

token :: Parser Text
token = label "token" $ do
  firstChar <- letterChar <|> char '_'
  restToken <- many $ letterChar <|> char '_' <|> digitChar
  return $ replaceType $ T.pack $ firstChar : restToken

qualifier :: Parser (Text, Position)
qualifier = label "qualifier"$  do
  space
  position' <- getSourcePos
  value <- token
  return (value, position')

variable :: Parser (Text, Position)
variable = label "variable" $ do
  space
  position' <- getSourcePos
  _ <- char '$'
  varName' <- token
  return (varName', position')
