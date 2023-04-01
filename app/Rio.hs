{-# LANGUAGE FlexibleContexts #-}

module Rio where

import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException, try)
import Data.Foldable (forM_)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

import Xml.Handle (XmlReaderMonad, getAttr, getPrompt, getTree, moveDown, moveUp)

data UserInput
    = IPrint
    | IUp
    | IDown Int
    | IAttr String
    | IExit

parse :: IO UserInput
parse = do
    line <- getLine
    let cmd = words line
    case head cmd of
        "print" -> return IPrint
        "up"    -> return IUp
        "down"  -> getDown cmd
        "attr"  -> getAttr cmd
        "exit"  -> return IExit
        _       -> error "Unknown command"
    where
        getDown cmd = let index = read $ cmd !! 1
                      in if index >= 0
                         then return $ IDown index
                         else error "Negative index"
        getAttr cmd = let attr = cmd !! 1
                      in if (not . null) attr
                         then return $ IAttr attr
                         else error "No attribute name"

tryParse :: IO (Maybe UserInput)
tryParse = do
    input <- liftIO (try parse :: IO (Either SomeException UserInput))
    case input of
        Left err    -> putStrLn ("Error: " ++ show err) >> return Nothing
        Right value -> return $ Just value

execute :: UserInput -> XmlReaderMonad ()
execute IPrint        = getTree >>= (liftIO . print)
execute IUp           = moveUp >> execute IPrint
execute (IDown index) = moveDown index >> execute IPrint
execute (IAttr attr)  = getAttr attr >>= (liftIO . putStrLn . unwords)
execute IExit         = liftIO exitSuccess

run :: XmlReaderMonad ()
run = do
    prompt <- getPrompt
    liftIO $ putStr prompt >> hFlush stdout
    input <- liftIO tryParse
    forM_ input execute
