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
tokenize ""            = []
tokenize s
    | isSpace $ head s = tokenize $ tail s
    | head s == '<'    = let (token, remainder) = tokenizeTag s
                         in token : tokenize remainder
    | otherwise        = case elemIndex '<' s of
        Nothing  -> [XmlLiteralToken s]
        Just pos -> XmlLiteralToken (take pos s) : tokenize (drop pos s)

-- | Parse data of format @\<...something here...>@
--   It can be an opening tag, a closing tag, or an @\<?xml?>@ tag
--   Returns token and the rest of the data
tokenizeTag :: String -> (XmlToken String, String)
tokenizeTag s = case elemIndex '>' s of
    Nothing  -> throw $ ParsingError $ "No closing > found: " ++ s
    Just pos -> (case s !! 1 of
            '/' -> closingTag pos
            '?' -> declarationTag pos
            _   -> openTag pos
        , remainder pos)
    where
        openTag        = XmlOpenTagToken . drop 1 . flip take s
        closingTag     = XmlClosingTagToken . drop 2 . flip take s
        declarationTag = XmlDeclarationTagToken . init . drop 2 . flip take s
        remainder      = flip drop s . succ
