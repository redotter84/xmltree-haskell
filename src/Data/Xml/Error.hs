module Data.Xml.Error where

import Control.Exception

data XmlError =
      ParsingError String
    | UnexpectedToken String
    | AttributesParsingError String
    | UnparsedData String
    deriving Show

instance Exception XmlError
