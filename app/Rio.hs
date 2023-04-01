{-# LANGUAGE FlexibleContexts #-}

module Rio where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import System.IO (hFlush, stdout)

import Xml.Handle

data UserInput
    = IPrint
    | IUp
    | IDown Int
    | IAttr String

parse :: IO UserInput
parse = do
    line <- getLine
    let cmd = words line
    case head cmd of
        "print" -> return IPrint
        "up"    -> return IUp
        "down"  -> return $ IDown (read (cmd !! 1) :: Int)
        "attr"  -> return $ IAttr (cmd !! 1)
        _       -> error "Unknown command"

execute :: (MonadReader AppEnv m, MonadIO m) => UserInput -> m ()
execute IPrint        = getTree >>= (liftIO . print)
execute IUp           = moveUp >> execute IPrint
execute (IDown index) = moveDown index >> execute IPrint
execute (IAttr attr)  = getAttr attr >>= (liftIO . print . unwords)

tryRun :: (MonadReader AppEnv m, MonadIO m) => m ()
tryRun = do
    prompt <- asks prompt
    liftIO $ putStr prompt >> hFlush stdout
    input <- liftIO parse
    execute input
    liftIO $ putStrLn "Done!"

run :: (MonadReader AppEnv m, MonadIO m) => m ()
run = tryRun
