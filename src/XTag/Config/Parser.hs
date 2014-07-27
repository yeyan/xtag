{-# LANGUAGE MultiParamTypeClasses #-}

module XTag.Config.Parser (parseConfig) where

import           Control.Applicative    ((<$>))
import           Data.Char
import qualified Data.Map               as Map
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.ByteString

quotedStr :: Parser String
quotedStr = do
    q <- char '"' <|> char '\''
    let escaped = char '\\' >> char q
    v <- many $ escaped <|> noneOf (q:"\n\r")
    char q
    return v

str :: Parser String
str = do
    let escaped = char '\\' >> oneOf " :"
    many1 $ escaped <|> noneOf " :\n\r"

values =
    (quotedStr <|> str) `sepBy` seperator
    where
        seperator = do
            spaces
            char ':'
            spaces
        spaces = do
            many $ oneOf " \t\f"

ident :: Parser String
ident =
    do
        c <- letter <|> char '_'
        cs <- many $ letter <|> digit <|> char '_' <|> char '.'
        return $ c:cs
    <?> "identifier"

comment :: Parser ()
comment =
    do
        char '#'
        skipMany $ noneOf "\r\n"
    <?> "comment"

eol :: Parser ()
eol =
    do
        oneOf "\n\r"
        return ()
    <?> "end of line"

item :: Parser (String, [String])
item = do
    key <- ident
    skipMany space
    char '='
    skipMany space
    vls <- values
    eol <|> comment
    return (key, vls)

line :: Parser (Maybe (String, [String]))
line = do
    skipMany space
    try (comment >> return Nothing) <|> (item >>= return . Just)

file = do
    lines <- many line
    return $ catMaybes lines

parseConfig path = do
    rlt <- parseFromFile file path
    return $ Map.fromList <$> rlt
