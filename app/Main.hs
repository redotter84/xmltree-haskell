module Main where

import System.Environment (getArgs)
import Data.XmlTree (parseXmlTree)
import Data.XmlToken

getFileName :: IO String
getFileName = getArgs >>= \args -> case length args of
    0 -> error "no files passed"
    otherwise -> return $ args !! 0

main :: IO ()
main = do
    rawXml <- (getFileName >>= readFile)
    putStrLn $ show $ parseXmlTree rawXml
