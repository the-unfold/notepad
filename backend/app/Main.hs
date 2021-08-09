{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- {-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import Connect (db)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int16, Int32, Int64)
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgConnect, pgDisconnect, useTPGDatabase)
import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.Protocol (PGConnection)
import Database.PostgreSQL.Typed.Query ( pgQuery, pgSQL, PGSimpleQuery )
import qualified Data.Aeson as JSON

import Web.Spock
  ( HasSpock (getState),
    SpockM,
    get,
    root,
    runSpock,
    spock,
    text,
    var,
    (<//>),
  )
import Web.Spock.Config
  ( PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
  )
import Database.PostgreSQL.Typed.Types (PGType)

useTPGDatabase db -- compile time connection

data MySession = EmptySession

data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
  do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8080 (spock spockCfg app)

listEvents :: IO [Int32]
listEvents = do
  conn <- pgConnect db
  res <- pgQuery conn [pgSQL| SELECT event_id from events; |]
  pgDisconnect conn
  pure res
  
a :: PGSimpleQuery JSON.Value
a = [pgSQL| SELECT body from events; |] 
-- test :: () -> Int
-- test = [pgSQL| SELECT event_id from events; |] 

app :: SpockM () MySession MyAppState ()
app =
  do
    get root $ text "Hello World!"
    get ("hello" <//> var) $ \name ->
      do
        (DummyAppState ref) <- getState
        visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i + 1, i + 1)
        text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
    get "hello/kitty" $ 
      do 
        evts <- liftIO listEvents
        text $ T.pack $ unwords $ map show evts