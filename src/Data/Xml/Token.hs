module Data.Xml.Token where

import Control.Exception (throw)
import Data.Char (isSpace)
import Data.List (elemIndex)

import Data.Xml.Error

data XmlToken a
    = XmlLiteralToken a
    | XmlOpenTagToken a
    | XmlClosingTagToken a
    | XmlDeclarationTagToken a
    deriving (Show, Eq)

-- | Make a sequence of tokens from raw XML data
tokenize :: String -> [XmlToken String]
tokenize ""                = []
tokenize input
    | isSpace $ head input = tokenize $ tail input
    | head input == '<'    = let (token, remainder) = tokenizeTag input
                             in token : tokenize remainder
    | otherwise            = case elemIndex '<' input of
        Nothing  -> [XmlLiteralToken input]
        Just pos -> XmlLiteralToken (take pos input) : tokenize (drop pos input)

-- | Parse data of format @\<...something here...>@
--   It can be an opening tag, a closing tag, or an @\<?xml?>@ tag
--   Returns token and the rest of the data
tokenizeTag :: String -> (XmlToken String, String)
tokenizeTag input = case elemIndex '>' input of
    Nothing  -> throw $ ParsingError $ "No closing > found: " ++ input
    Just pos -> (case input !! 1 of
            '/' -> closingTag pos
            '?' -> declarationTag pos
            _   -> openTag pos
        , remainder pos)
    where
        openTag        = XmlOpenTagToken . drop 1 . flip take input
        closingTag     = XmlClosingTagToken . drop 2 . flip take input
        declarationTag = XmlDeclarationTagToken . init . drop 2 . flip take input
        remainder      = flip drop input . succ
