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

import Connect
import Control.Monad.Trans (MonadIO (liftIO))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int16, Int32, Int64)
import qualified Data.Text as T
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Protocol (PGConnection)
import Database.PostgreSQL.Typed.Query
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

useTPGDatabase db -- compile time connection

data MySession = EmptySession

data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
  do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8080 (spock spockCfg app)

listEvents :: IO [(Int32, T.Text)]
listEvents = do
  conn <- pgConnect db
  pgQuery conn [pgSQL| SELECT event_id, body from events; |]

app :: SpockM () MySession MyAppState ()
app =
  do
    get root $ text "Hello World!"
    get ("hello" <//> var) $ \name ->
      do
        (DummyAppState ref) <- getState
        visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i + 1, i + 1)
        text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))