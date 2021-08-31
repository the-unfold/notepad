{-# LANGUAGE DataKinds #-}

module HttpServer (handleRequests) where

import Control.Concurrent.STM.TBChan (TBChan)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Except (runExceptT)
import Data.Int (Int32)
import DomainEvent qualified
import EventRegistrator (insertEvent)
import HttpTypes
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Queries qualified
import Servant
import Types.WithUuid

handleRequests :: TBChan () -> IO ()
handleRequests chan = do
  let port = (8000 :: Int)
  putStrLn $ "Starting server at port " <> show port
  run port (app chan)

app :: TBChan () -> Application
app chan =
  logStdoutDev
    . cors (const $ Just corsPolicy)
    $ serve notepadApi (server chan)
    -- catch error and if it is like "somethig wend wrong" then respond with 500 error manually
  where
    -- Note: Content-Type header is necessary for POST requests
    corsPolicy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}

notepadApi :: Proxy NotepadApi
notepadApi = Proxy

type NotepadApi =
  "users" :> ReqBody '[JSON] (WithUuid UserRegisterPayload) :> PostNoContent
    :<|> "notes" :> Get '[JSON] [Note]
    :<|> "notes" :> QueryParam' '[Required] "id" Int32 :> Get '[JSON] Note
    :<|> "notes" :> "create" :> ReqBody '[JSON] (WithUuid NoteCreatePayload) :> PostNoContent
    :<|> "notes" :> "update" :> ReqBody '[JSON] (WithUuid NoteUpdatePayload) :> PostNoContent

server :: TBChan () -> Server NotepadApi
server chan =
  registerUser
    :<|> getNotes
    :<|> getNote
    :<|> createNote
    :<|> updateNote
  where
    registerUser :: WithUuid UserRegisterPayload -> Handler NoContent
    registerUser WithUuid {uuid, payload} = do
      insertionResult <- liftIO $ runExceptT $ insertEvent chan uuid (DomainEvent.UserRegistered (email payload))
      case insertionResult of
        Right _ -> pure NoContent
        Left _ -> throwError err500

    getNotes :: Handler [Note]
    getNotes = do
      let userId = 1
      liftIO $ Queries.queryNotes userId

    getNote :: Int32 -> Handler Note
    getNote noteId = do
      let userId = 1
      liftIO $ Queries.queryNote noteId userId

    createNote :: WithUuid NoteCreatePayload -> Handler NoContent
    createNote WithUuid {uuid, payload} = do
      let userId = 1
      insertionResult <- liftIO $ runExceptT $ insertEvent chan uuid (DomainEvent.NoteCreated userId (content (payload :: NoteCreatePayload)))
      case insertionResult of
        Right _ -> pure NoContent
        Left _ -> throwError err500

    updateNote :: WithUuid NoteUpdatePayload -> Handler NoContent
    updateNote WithUuid {uuid, payload} = do
      let userId = 1
      insertionResult <- liftIO $ runExceptT $ insertEvent chan uuid (DomainEvent.NoteUpdated userId (noteId (payload :: NoteUpdatePayload)) (content (payload :: NoteUpdatePayload)))
      case insertionResult of
        Right _ -> pure NoContent
        Left _ -> throwError err500
