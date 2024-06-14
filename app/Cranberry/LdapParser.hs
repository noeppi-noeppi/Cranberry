{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

module Cranberry.LdapParser where

import Data.Either (Either)
import Data.Text (Text)
import Data.List.NonEmpty
import Data.Attoparsec.Text
import Control.Applicative (Alternative, (<*>), (<*), (*>), (<|>), many)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Ldap.Client as Ldap

deriving instance Show Ldap.Filter

many1n :: Parser a -> Parser (NonEmpty a)
many1n parser = do
  result <- many1 parser
  case result of
    x : xs -> return $ x :| xs
    [] -> fail ""

attributeParser :: Parser Ldap.Attr
attributeParser = (Ldap.Attr . T.pack <$>) $ (:) <$> letter <*> many keychar
  where keychar = letter <|> digit <|> char '-'

valueParser :: Parser Ldap.AttrValue
valueParser = (TE.encodeUtf8 . T.pack) <$> valueWord
  where valueWord :: Parser String
        valueWord = many1 $ satisfy $ not . forbidden
        forbidden :: Char -> Bool
        forbidden chr = chr == '(' || chr == ')' || chr == '&' || chr == '|'
                     || chr == '*' || chr == '/' || chr == '\\'

mkParenParser :: Parser a -> Parser a
mkParenParser parser = skipSpace *> string "(" *> skipSpace *> parser <* skipSpace <* string ")" <* skipSpace 

mkJoinParser :: Text -> Parser a -> Parser a
mkJoinParser char parser = mkParenParser $ string char *> skipSpace *> parser

mkRelParser :: Text -> Parser (Ldap.Attr, Ldap.AttrValue)
mkRelParser rel = mkParenParser $ (into <$> attributeParser) <*> (skipSpace *> string rel *> skipSpace *> valueParser)
  where into :: a -> b -> (a, b)
        into a b = (a, b)

parseLdapFilter :: String -> Maybe Ldap.Filter
parseLdapFilter filterStr = case parseOnly (filterParser <* endOfInput) (T.pack filterStr) of
  Left err -> Nothing
  Right filter -> Just filter

testLdapFilter :: String -> Either String Ldap.AttrValue
testLdapFilter filterStr = parseOnly (valueParser <* endOfInput) (T.pack filterStr)

filterParser :: Parser Ldap.Filter
filterParser = notFilterParser
           <|> andFilterParser
           <|> orFilterParser
           <|> existenceFilterParser
           <|> eqFilterParser
           <|> geqFilterParser
           <|> leqFilterParser
           <|> proxFilterParser

notFilterParser :: Parser Ldap.Filter
notFilterParser = mkJoinParser "!" $ (Ldap.Not <$> filterParser)

andFilterParser :: Parser Ldap.Filter
andFilterParser = mkJoinParser "&" $ (Ldap.And <$> many1n filterParser)

orFilterParser :: Parser Ldap.Filter
orFilterParser = mkJoinParser "|" $ (Ldap.And <$> many1n filterParser)

existenceFilterParser :: Parser Ldap.Filter
existenceFilterParser = mkParenParser $ (Ldap.Present <$> attributeParser) <* string "=" <* skipSpace <* string "*"

eqFilterParser :: Parser Ldap.Filter
eqFilterParser = uncurry (Ldap.:=) <$> mkRelParser "="

geqFilterParser :: Parser Ldap.Filter
geqFilterParser = uncurry (Ldap.:>=) <$> mkRelParser ">="

leqFilterParser :: Parser Ldap.Filter
leqFilterParser = uncurry (Ldap.:<=) <$> mkRelParser "<="

proxFilterParser :: Parser Ldap.Filter
proxFilterParser = uncurry (Ldap.:~=) <$> mkRelParser "~="
