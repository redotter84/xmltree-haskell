module Data.Xml.Tree where

import Control.Exception (throw)
import Data.List (intercalate)

import Data.Xml.Error

data XmlTree
    = XmlEmpty
    | XmlLiteral String
    | XmlTag String [XmlAttribute] [XmlTree]
    deriving (Eq)

data XmlAttribute = XmlAttribute String String
    deriving (Eq)

-- | Display an XML tree in a human-readable format
showTree :: String -> XmlTree -> String
showTree prefix XmlEmpty = "nothing"
showTree prefix (XmlLiteral x) = prefix ++ x
showTree prefix (XmlTag name attrs children) = unwords [prefix ++ wrap "<" ">" name, showAttrs attrs, showChildren children]
    where
        showChildren = concatMap (showTree $ prefix ++ "│    ")
        showAttrs = wrapAttrs . intercalate ", " . map (\(XmlAttribute key value) -> intercalate ": " [key, value])
        wrap open close = (open ++) . (++ close)
        wrapAttrs [] = ""
        wrapAttrs attrs = wrap "{" "}" attrs

instance Show (XmlTree) where
    show = drop 1 . showTree "\n"

-- | Find tree child by index
findXmlTreeChild :: Int -> XmlTree -> XmlTree
findXmlTreeChild _ XmlEmpty = throw ImpossibleError
findXmlTreeChild _ (XmlLiteral _) = throw $ NoChildFound "Literals have no children"
findXmlTreeChild index (XmlTag _ _ children)
    | index < length children = children !! index
    | otherwise               = throw $ NoChildFound "No child found with given index"

-- | Find tree attribute by name
findXmlTreeAttribute :: String -> XmlTree -> [String]
findXmlTreeAttribute _ XmlEmpty = throw ImpossibleError
findXmlTreeAttribute _ (XmlLiteral _) = throw $ NoAttributeFound "Literals have no attributes"
findXmlTreeAttribute name (XmlTag _ attrs _) = map getValue $ filter match attrs
    where
        match (XmlAttribute attr _) = attr == name
        getValue (XmlAttribute _ value) = value
