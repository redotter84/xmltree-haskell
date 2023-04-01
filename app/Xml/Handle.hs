{-# LANGUAGE FlexibleContexts #-}

module Xml.Handle where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Data.Xml.Tree

data XmlTreeWithParent = WithParent XmlTree (Maybe XmlTreeWithParent)

newtype XmlHandle = XmlHandle {
    getTreeRef :: IORef XmlTreeWithParent
}
newXmlHandle :: XmlTree -> IO XmlHandle
newXmlHandle tree = XmlHandle <$> newIORef (WithParent tree Nothing)

newtype AppEnv = AppEnv {
    xmlHandle :: XmlHandle
}
newAppEnv :: XmlTree -> IO AppEnv
newAppEnv tree = AppEnv <$> newXmlHandle tree

getTreeHandle :: (MonadReader AppEnv m, MonadIO m) => m (IORef XmlTreeWithParent)
getTreeHandle = asks $ getTreeRef . xmlHandle

getTree :: (MonadReader AppEnv m, MonadIO m) => m XmlTree
getTree = do
    handle <- getTreeHandle
    stored <- liftIO $ readIORef handle
    let (WithParent tree _) = stored
    return tree

moveUp :: (MonadReader AppEnv m, MonadIO m) => m ()
moveUp = do
    handle <- getTreeHandle
    stored <- liftIO $ readIORef handle
    let (WithParent _ parent) = stored
    case parent of
        Nothing         -> error "No way up"
        Just parentTree -> liftIO $ writeIORef handle parentTree

moveDown :: (MonadReader AppEnv m, MonadIO m) => Int -> m ()
moveDown index = do
    handle <- getTreeHandle
    stored <- liftIO $ readIORef handle
    let treeWithParent@(WithParent tree _) = stored
    let child = findXmlTreeChild index tree
    let newStored = WithParent child $ Just treeWithParent
    liftIO $ writeIORef handle newStored

getAttr :: (MonadReader AppEnv m, MonadIO m) => String -> m [String]
getAttr attr = do
    handle <- getTreeHandle
    stored <- liftIO $ readIORef handle
    let (WithParent tree _) = stored
    return $ findXmlTreeAttribute attr tree
