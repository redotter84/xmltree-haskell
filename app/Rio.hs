{-# LANGUAGE FlexibleContexts #-}

module Rio where

import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException, try)
import Data.Foldable (forM_)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

import Data.Xml.Tree (XmlTree)
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
    let checkCmd required oper = if length cmd < required
                                 then error "Not enough args"
                                 else oper
    case head cmd of
        "print" -> return IPrint
        "up"    -> return IUp
        "down"  -> checkCmd 2 $ return $ IDown $ read $ cmd !! 1
        "attr"  -> checkCmd 2 $ return $ IAttr $ cmd !! 1
        "exit"  -> return IExit
        _       -> error "Unknown command"

tryParse :: IO (Maybe UserInput)
tryParse = do
    tryResult <- liftIO (try parse :: IO (Either SomeException UserInput))
    case tryResult of
        Left err    -> putStrLn ("Error: " ++ show err) >> return Nothing
        Right input -> return $ Just input

tryPrint :: XmlReaderMonad ()
tryPrint = do
    tree <- getTree
    let function = print tree
    tryResult <- liftIO (try function :: IO (Either SomeException ()))
    case tryResult of
        Left err -> liftIO $ putStrLn ("Error: " ++ show err)
        Right _  -> return ()

execute :: UserInput -> XmlReaderMonad ()
execute IPrint        = tryPrint
execute IUp           = moveUp >> tryPrint
execute (IDown index) = moveDown index >> tryPrint
execute (IAttr attr)  = getAttr attr >>= (liftIO . putStrLn . unwords)
execute IExit         = liftIO exitSuccess

run :: XmlReaderMonad ()
run = do
    prompt <- getPrompt
    liftIO $ putStr prompt >> hFlush stdout
    input <- liftIO tryParse
    forM_ input execute
