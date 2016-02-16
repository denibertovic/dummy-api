{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Control.Concurrent          (threadDelay)
import           Control.Exception           (SomeException, catch)
import           Control.Exception.Extra     (retry)
import           Control.Retry

import           Dummy.Api.Api               (app)
import           Dummy.Api.Config            (Config (..), Environment (..),
                                              defaultConfig, makePool,
                                              setLogger)

import           Dummy.Api.Models            (doMigrations)


main :: IO ()
main = do
    env  <- lookupSetting "ENV" Test
    port <- lookupSetting "PORT" 8000
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getEnv = env }
        logger = setLogger env
    retry 5 $ retryDbConnection $ runSqlPool doMigrations pool
    run port $ logger $ app cfg

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
retryDbConnection :: IO a -> IO a
retryDbConnection action = catch action $ \(_ :: SomeException) -> do
    -- TODO: Got to figure out what exact exception to catch
    putStrLn "Failed to connect to db. Retrying..."
    threadDelay 5000000
    action

