{-# LANGUAGE FlexibleContexts #-}

module Xml.Handle where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Data.Xml.Tree (XmlTree, findXmlTreeAttribute, findXmlTreeChild)

data XmlTreeWithParent = WithParent XmlTree (Maybe XmlTreeWithParent)

newtype XmlHandle = XmlHandle {
    getTreeRef :: IORef XmlTreeWithParent
}
newXmlHandle :: XmlTree -> IO XmlHandle
newXmlHandle tree = XmlHandle <$> newIORef (WithParent tree Nothing)

data AppEnv = AppEnv {
    prompt    :: String,
    xmlHandle :: XmlHandle
}
newAppEnv :: String -> XmlTree -> IO AppEnv
newAppEnv invite tree = AppEnv invite <$> newXmlHandle tree

type XmlReaderMonad a = ReaderT AppEnv IO a

getPrompt :: XmlReaderMonad String
getPrompt = asks prompt

getTreeHandle :: XmlReaderMonad (IORef XmlTreeWithParent)
getTreeHandle = asks $ getTreeRef . xmlHandle

getTree :: XmlReaderMonad XmlTree
getTree = do
    handle <- getTreeHandle
    stored <- liftIO $ readIORef handle
    let (WithParent tree _) = stored
    return tree

moveUp :: XmlReaderMonad ()
moveUp = do
    handle <- getTreeHandle
    stored <- liftIO $ readIORef handle
    let (WithParent _ parent) = stored
    case parent of
        Nothing         -> error "No way up"
        Just parentTree -> liftIO $ writeIORef handle parentTree

moveDown :: Int -> XmlReaderMonad ()
moveDown index = do
    handle <- getTreeHandle
    stored <- liftIO $ readIORef handle
    let treeWithParent@(WithParent tree _) = stored
    let child = findXmlTreeChild index tree
    let newStored = WithParent child $ Just treeWithParent
    liftIO $ writeIORef handle newStored

getAttr :: String -> XmlReaderMonad [String]
getAttr attr = do
    handle <- getTreeHandle
    stored <- liftIO $ readIORef handle
    let (WithParent tree _) = stored
    return $ findXmlTreeAttribute attr tree
