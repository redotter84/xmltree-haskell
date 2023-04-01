module Data.XmlTree where

import Data.Char (isSpace)
import Data.List (elemIndex, intercalate)
import Data.XmlToken

type Attribute = (String, String)

type Attributes = [Attribute]

data XmlTree a = XmlLiteral a | XmlTag String Attributes [XmlTree a]

showTree :: (Show a) => String -> XmlTree a -> String
showTree prefix (XmlLiteral x) = prefix ++ show x
showTree prefix (XmlTag name attrs children) = intercalate " " [prefix ++ name, showAttrs attrs, showChildren children]
    where
        showChildren = concat . map (showTree $ prefix ++ "│   ")
        showAttrs = wrapAttrs . intercalate ", " . map (\(k, v) -> intercalate ": " [k, v])
        wrap open close = (open ++) . (++ close)
        wrapAttrs [] = ""
        wrapAttrs attrs = wrap "{" "}" attrs

instance (Show a) => Show (XmlTree a) where
    show = showTree "\n"

parseAttrs :: String -> Attributes
parseAttrs "" = []
parseAttrs s
    | isSpace (s !! 0) = parseAttrs $ tail s
    | otherwise = case elemIndex '=' s of
        Nothing -> []
        Just sep ->
            let (value, remainder) = next sep
            in (key sep, value) : parseAttrs remainder
    where
        key sep = take sep s
        afterSep sep = drop (sep + 2) s
        next sep =
            let valueWithRemainder = afterSep sep
            in case elemIndex '"' $ valueWithRemainder of
                Nothing -> (valueWithRemainder, "")
                Just pos -> (take pos valueWithRemainder, drop (succ pos) valueWithRemainder)

parseOpenTag :: XmlToken String -> (String, Attributes)
parseOpenTag (XmlOpenTagToken tag) = (name, attrs)
    where
        name = (words tag) !! 0
        attrs = case elemIndex ' ' tag of
            Nothing -> []
            Just position -> parseAttrs $ drop (succ position) tag

findSubtree :: Int -> [XmlToken a] -> ([XmlToken a], [XmlToken a])
findSubtree 0 tokens = ([], tokens)
findSubtree _ [] = ([], [])
findSubtree balance (token : tokens) = (token : tree, remainder)
    where
        changeBalance = case token of
            XmlLiteralToken _ -> id
            XmlOpenTagToken _ -> succ
            XmlClosingTagToken _ -> pred
        (tree, remainder) = findSubtree (changeBalance balance) tokens

parseXmlForestFromTokens :: [XmlToken String] -> [XmlTree String]
parseXmlForestFromTokens [] = []
parseXmlForestFromTokens tokens = tree : parseXmlForestFromTokens remainder
    where
        (tree, remainder) = parseXmlTreeFromTokens tokens

parseXmlTagFromTokens :: XmlToken String -> [XmlToken String] -> (XmlTree String, [XmlToken String])
parseXmlTagFromTokens openTagToken tokens = (XmlTag name attrs children, remainder)
    where
        (name, attrs) = parseOpenTag openTagToken
        (treeTokens, remainder) = findSubtree 1 tokens
        children = parseXmlForestFromTokens $ init treeTokens

parseXmlTreeFromTokens :: [XmlToken String] -> (XmlTree String, [XmlToken String])
parseXmlTreeFromTokens (token : tokens) = case token of
    XmlLiteralToken x -> (XmlLiteral x, tokens)
    XmlOpenTagToken x -> parseXmlTagFromTokens token tokens
    otherwise         -> parseXmlTreeFromTokens tokens

parseXmlTree :: String -> XmlTree String
parseXmlTree = fst . parseXmlTreeFromTokens . tokenize
