module Main where

import System.Environment (getArgs)
import Data.Xml.Tree (parseXml)

getFileName :: IO String
getFileName = getArgs >>= \args -> case length args of
    0 -> error "no files passed"
    _ -> return $ head args

main :: IO ()
main = do
    rawXml <- getFileName >>= readFile
    putStrLn $ show $ parseXml rawXml
