{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Operator
  ( parseAnonymousQuery
  , parseOperator
  ) where

import           Control.Applicative                       ((<|>))
import           Data.Functor                              (($>))
import           Data.Morpheus.Parser.Body                 (entries)
import           Data.Morpheus.Parser.Internal             (Parser)
import           Data.Morpheus.Parser.Primitive            (token, variable)
import           Data.Morpheus.Parser.Terms                (nonNull, parseAssignment, parseChar, parseMaybeTuple)
import           Data.Morpheus.Types.Internal.AST.Operator (Operator (..), Operator' (..), RawOperator, RawOperator',
                                                            Variable (..))
import           Data.Morpheus.Types.Internal.Data         (DataTypeWrapper (..))
import           Data.Text                                 (Text)
import           Text.Megaparsec                           (label, getSourcePos, try, (<?>))
import           Text.Megaparsec.Char                      (char, space, string)

wrapMock :: Parser ([DataTypeWrapper], Text)
wrapMock = space >> token >>= \x -> pure ([], x)

insideList :: Parser ([DataTypeWrapper], Text)
insideList = do
  space
  _ <- char '['
  space
  (list, name) <- try wrapMock <|> insideList
  space
  nonNull' <- nonNull
  space
  _ <- char ']'
  return ((ListType : nonNull') ++ list, name)

wrappedSignature :: Parser ([DataTypeWrapper], Text)
wrappedSignature = try insideList <|> wrapMock

operatorArgument :: Parser (Text, Variable)
operatorArgument = label "operatorArgument" $ do
  ((name', position'), (wrappers', type')) <- parseAssignment variable wrappedSignature
  nonNull' <- nonNull
  pure
    ( name'
    , Variable
        { variableType = type'
        , isVariableRequired = 0 < length nonNull'
        , variableTypeWrappers = nonNull' ++ wrappers'
        , variablePosition = position'
        })

parseOperator :: Parser RawOperator
parseOperator = label "operator" $ do
  space
  pos <- getSourcePos
  kind' <- operatorKind
  parseChar ' '
  space
  operatorName' <- token
  variables <- parseMaybeTuple operatorArgument
  space
  sel <- entries
  pure (kind' $ Operator' operatorName' variables sel pos)

parseAnonymousQuery :: Parser RawOperator
parseAnonymousQuery = label "AnonymousQuery" $ do
  space
  position' <- getSourcePos
  selection' <- entries
  pure (Query $ Operator' "" [] selection' position') <?> "can't parse AnonymousQuery"

operatorKind :: Parser (RawOperator' -> RawOperator)
operatorKind = label "operatorKind" $
  (string "query" $> Query)
  <|> (string "mutation" $> Mutation)
  <|> (string "subscription" $> Subscription)
