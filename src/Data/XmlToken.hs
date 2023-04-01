module Data.XmlToken where

import Data.Char (isSpace)
import Data.List (elemIndex)

data XmlToken a = XmlLiteralToken a
                  | XmlOpenTagToken a
                  | XmlClosingTagToken a
                  | XmlDeclarationTagToken a
    deriving (Show)

tokenizeTag :: String -> (XmlToken String, String)
tokenizeTag s = case elemIndex '>' s of
    Nothing -> (XmlOpenTagToken $ drop 1 s, "")
    Just position -> ((case s !! 1 of
            '/' -> closingTag position
            '?' -> declarationTag position
            otherwise -> openTag position
        ), remainder position)
    where
        openTag = XmlOpenTagToken . drop 1 . flip take s
        closingTag = XmlClosingTagToken . drop 2 . flip take s
        declarationTag = XmlDeclarationTagToken . drop 2 . flip take s
        remainder = flip drop s . succ

tokenize :: String -> [XmlToken String]
tokenize "" = []
tokenize s
    | isSpace (s !! 0) = tokenize $ tail s
    | s !! 0 == '<'    = let (token, remainder) = tokenizeTag s in
                             token : tokenize remainder
    | otherwise = case elemIndex '<' s of
        Nothing -> [XmlLiteralToken s]
        Just position -> (XmlLiteralToken $ take position s) : (tokenize $ drop position s)
