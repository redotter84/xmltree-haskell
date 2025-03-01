module Data.Xml.Parse where

import Control.Exception (throw)
import Data.Char (isSpace)
import Data.List (elemIndex)

import Data.Xml.Error
import Data.Xml.Token
import Data.Xml.Tree

-- | Parse an XML tree from a raw text
parseXml :: String -> XmlTree
parseXml raw = case remainder of
    [] -> tree
    _  -> throw $ UnparsedData $ "Some data was left unparsed: " ++ show remainder
    where
        (tree, remainder) = (makeXmlTreeFromTokens . tokenize) raw

-- | Parse tag attributes
parseAttrs :: String -> [XmlAttribute]
parseAttrs ""              = []
parseAttrs input
    | isSpace $ head input = (parseAttrs . tail) input
    | otherwise            = case elemIndex '=' input of
        Nothing  -> throw $ AttributesParsingError $ "No `=` found: " ++ input
        Just sep ->
            if input !! (sep + 1) == '"'
            then
                let (value, remainder) = next sep
                in XmlAttribute (key sep) value : parseAttrs remainder
            else
                throw $ AttributesParsingError $ "No `\"` found" ++ input
    where
        key sep      = take sep input
        afterSep sep = drop (sep + 2) input
        next sep     =
            let valueWithRemainder = afterSep sep
            in case elemIndex '"' valueWithRemainder of
                Nothing  -> throw $ AttributesParsingError $ "No `\"` found" ++ valueWithRemainder
                Just pos -> (take pos valueWithRemainder, drop (succ pos) valueWithRemainder)

-- | Parse name and attributes from XmlOpenTagToken. Children will be found later
parseOpenTagToken :: XmlToken String -> (String, [XmlAttribute])
parseOpenTagToken (XmlOpenTagToken tag) = (name, attrs)
    where
        name = head $ words tag
        attrs = case elemIndex ' ' tag of
            Nothing  -> []
            Just pos -> parseAttrs $ drop (succ pos) tag

-- | Find a tag subtree using an approach similar to bracket sequences
findSubtree :: Int -> [XmlToken String] -> ([XmlToken String], [XmlToken String])
findSubtree 0 tokens                 = ([], tokens)
findSubtree _ []                     = ([], [])
findSubtree balance (token : tokens) = (token : tree, remainder)
    where
        changeBalance     = case token of
            XmlLiteralToken _    -> id
            XmlOpenTagToken _    -> succ
            XmlClosingTagToken _ -> pred
            _                    -> throw $ UnexpectedToken $ show token
        (tree, remainder) = findSubtree (changeBalance balance) tokens

-- | Given tokens, make as many subtrees (either tags or literals) as possible
makeXmlForestFromTokens :: [XmlToken String] -> [XmlTree]
makeXmlForestFromTokens []     = []
makeXmlForestFromTokens tokens = tree : makeXmlForestFromTokens remainder
    where
        (tree, remainder) = makeXmlTreeFromTokens tokens

-- | Make an XML tree for <tag>...</tag> structure
makeXmlTagFromTokens :: XmlToken String -> [XmlToken String] -> (XmlTree, [XmlToken String])
makeXmlTagFromTokens openTagToken tokens = case lastToken of
    XmlClosingTagToken x -> if name == x
        then (XmlTag name attrs children, remainder)
        else throw $ UnexpectedToken $ show "Expected tag " ++ name ++ ", got " ++ show lastToken
    _                    -> throw $ UnexpectedToken $ show lastToken
    where
        (name, attrs)           = parseOpenTagToken openTagToken
        (treeTokens, remainder) = findSubtree 1 tokens
        children                = (makeXmlForestFromTokens . init) treeTokens
        lastToken               = last treeTokens

-- | Make an XML tree (can be just a subtree) from tokens
--   It returns the tree itself and unparsed tokens
makeXmlTreeFromTokens :: [XmlToken String] -> (XmlTree, [XmlToken String])
makeXmlTreeFromTokens (token : tokens) = case token of
    XmlLiteralToken x        -> (XmlLiteral x, tokens)
    XmlOpenTagToken _        -> makeXmlTagFromTokens token tokens
    XmlDeclarationTagToken _ -> makeXmlTreeFromTokens tokens
    _                        -> throw $ UnexpectedToken $ show token
