module Data.Xml.Error where

import Control.Exception

data XmlError
    = ParsingError String
    | UnexpectedToken String
    | AttributesParsingError String
    | UnparsedData String
    | NoChildFound String
    | NoAttributeFound String
    | ImpossibleError
    deriving Show

instance Exception XmlError
