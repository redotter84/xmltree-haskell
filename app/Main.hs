module Main where

import Control.Monad (forever)
import Control.Monad.Reader (runReaderT)
import System.Environment (getArgs)

import Data.Xml.Parse (parseXml)
import Rio (run)
import Xml.Handle (newAppEnv)

getFileName :: IO String
getFileName = getArgs >>= \args -> case length args of
    0 -> error "no files passed"
    _ -> return $ head args

main :: IO ()
main = do
    raw <- getFileName >>= readFile
    let tree = parseXml raw
    print $ tree
    newAppEnv tree >>= runReaderT (forever run)
