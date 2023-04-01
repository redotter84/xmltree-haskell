module Data.Xml.Parse where

import Control.Exception (throw)
import Data.Char (isSpace)
import Data.List (elemIndex, intercalate)
import Data.Xml.Error
import Data.Xml.Token

type Attribute = (String, String)

type Attributes = [Attribute]

-- | Data structure for XML trees
data Show a => XmlTree a =
      XmlLiteral a
    | XmlTag String Attributes [XmlTree a]
    deriving (Eq)

-- | Display an XML tree in a human-readable format
showTree :: (Show a) => String -> XmlTree a -> String
showTree prefix (XmlLiteral x) = prefix ++ show x
showTree prefix (XmlTag name attrs children) = unwords [prefix ++ name, showAttrs attrs, showChildren children]
    where
        showChildren = concatMap (showTree $ prefix ++ "│   ")
        showAttrs = wrapAttrs . intercalate ", " . map (\(k, v) -> intercalate ": " [k, v])
        wrap open close = (open ++) . (++ close)
        wrapAttrs [] = ""
        wrapAttrs attrs = wrap "{" "}" attrs

instance (Show a) => Show (XmlTree a) where
    show = showTree "\n"

-- | Parse an XML tree from a raw text
parseXml :: String -> XmlTree String
parseXml raw = case remainder of
    [] -> tree
    _  -> throw $ UnparsedData $ "Some data was left unparsed: " ++ show remainder
    where
        (tree, remainder) = (makeXmlTreeFromTokens . tokenize) raw

-- | Parse tag attributes (e.g. for @\<p class="test" style="meh">@ attributes are @{class, test), (style, meh)@)
parseAttrs :: String -> Attributes
parseAttrs "" = []
parseAttrs s
    | isSpace $ head s = (parseAttrs . tail) s
    | otherwise = case elemIndex '=' s of
        Nothing -> throw $ AttributesParsingError $ "No `=` found: " ++ s
        Just sep ->
            if s !! (sep + 1) == '"'
            then
                let (value, remainder) = next sep
                in (key sep, value) : parseAttrs remainder
            else
                throw $ AttributesParsingError $ "No `\"` found" ++ s
    where
        key sep = take sep s
        afterSep sep = drop (sep + 2) s
        next sep =
            let valueWithRemainder = afterSep sep
            in case elemIndex '"' valueWithRemainder of
                Nothing -> throw $ AttributesParsingError $ "No `\"` found" ++ valueWithRemainder
                Just pos -> (take pos valueWithRemainder, drop (succ pos) valueWithRemainder)

-- | Parse name and attributes from XmlOpenTagToken. Children will be found later
parseOpenTagToken :: XmlToken String -> (String, Attributes)
parseOpenTagToken (XmlOpenTagToken tag) = (name, attrs)
    where
        name = head $ words tag
        attrs = case elemIndex ' ' tag of
            Nothing -> []
            Just position -> parseAttrs $ drop (succ position) tag

-- | Find a tag subtree using an approach similar to bracket sequences
findSubtree :: Int -> [XmlToken String] -> ([XmlToken String], [XmlToken String])
findSubtree 0 tokens = ([], tokens)
findSubtree _ [] = ([], [])
findSubtree balance (token : tokens) = (token : tree, remainder)
    where
        changeBalance = case token of
            XmlLiteralToken _    -> id
            XmlOpenTagToken _    -> succ
            XmlClosingTagToken _ -> pred
            _                    -> throw $ UnexpectedToken $ show token
        (tree, remainder) = findSubtree (changeBalance balance) tokens

-- | Given tokens, make as many subtrees (either tags or literals) as possible
makeXmlForestFromTokens :: [XmlToken String] -> [XmlTree String]
makeXmlForestFromTokens [] = []
makeXmlForestFromTokens tokens = tree : makeXmlForestFromTokens remainder
    where
        (tree, remainder) = makeXmlTreeFromTokens tokens

-- | Make an XML tree for <tag>...</tag> structure
makeXmlTagFromTokens :: XmlToken String -> [XmlToken String] -> (XmlTree String, [XmlToken String])
makeXmlTagFromTokens openTagToken tokens = case lastToken of
    XmlClosingTagToken x -> if name == x
        then (XmlTag name attrs children, remainder)
        else throw $ UnexpectedToken $ show "Expected tag " ++ name ++ ", got " ++ show lastToken
    _                    -> throw $ UnexpectedToken $ show lastToken
    where
        (name, attrs) = parseOpenTagToken openTagToken
        (treeTokens, remainder) = findSubtree 1 tokens
        children = (makeXmlForestFromTokens . init) treeTokens
        lastToken = last treeTokens

-- | Make an XML tree (can be just a subtree) from tokens
--   It returns the tree itself and unparsed tokens
makeXmlTreeFromTokens :: [XmlToken String] -> (XmlTree String, [XmlToken String])
makeXmlTreeFromTokens (token : tokens) = case token of
    XmlLiteralToken x        -> (XmlLiteral x, tokens)
    XmlOpenTagToken _        -> makeXmlTagFromTokens token tokens
    XmlDeclarationTagToken _ -> makeXmlTreeFromTokens tokens
    _                        -> throw $ UnexpectedToken $ show token
