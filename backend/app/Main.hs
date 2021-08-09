{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Connect (db)
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgConnect, pgDisconnect, useTPGDatabase)
import Database.PostgreSQL.Typed.Protocol (PGConnection)
import Database.PostgreSQL.Typed.Query (PGSimpleQuery, pgQuery, pgSQL)
import Database.PostgreSQL.Typed.Types (PGType)
import GHC.Generics (Generic)
import Network.HTTP.Types (status500)
import Web.Spock
  ( HasSpock (getState),
    SpockM,
    get,
    response,
    root,
    runSpock,
    setStatus,
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

-- | List of all our domain events in a single event log
data DomainEvent
  = KittyPoela
  | KittyNasrala {count :: Int, sound :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data MySession = EmptySession

newtype MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
  do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8080 (spock spockCfg app)

runQuery :: PGSimpleQuery a -> IO [a]
runQuery q = do
  conn <- pgConnect db
  res <- pgQuery conn q
  pgDisconnect conn
  pure res

listEvents :: IO (Aeson.Result [DomainEvent])
listEvents = do
  traverse Aeson.fromJSON <$> (runQuery [pgSQL| SELECT body from events; |] :: IO [Aeson.Value])

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
        case evts of
          Aeson.Error err -> setStatus status500
          Aeson.Success xs -> text $ T.pack $ unwords $ map show xs
