{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module EventProcessor (processEventsInLoop) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, readTBChan)
import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.UUID qualified as UUID
import Database
  ( includesText,
    runQueryWithNewConnection,
    runQueryWithNewConnection_,
  )
import Database.PostgreSQL.Typed (PGError)
import Database.PostgreSQL.Typed.Query (pgSQL)
import DomainEvent (DomainEvent (..))
import Fmt (fmt, fmtLn, (+|), (|+))
import GHC.Generics (Generic)

data ProcessingError
  = InternalProcessingError
  | InvalidOperation {explaination :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- а юнит ли? возвращать результат обработки - не обязательно правильное действие.
-- Но, с другой стороны, мы хотели бы иметь унифицированный интерфейс для регистрации ответов.
-- Думаю, пока не будем его унифицировать, пусть каждая IO фиксирует ответ отдельно
processEvent :: DomainEvent -> ExceptT ProcessingError IO ()
processEvent UserRegistered {email} = do
  -- command processing
  let transformError :: PGError -> ProcessingError
      transformError e
        | includesText "unique_user_email" e = InvalidOperation "User with such email already exists."
        | otherwise = InternalProcessingError
  withExceptT transformError . ExceptT . try $
  -- event processing
    runQueryWithNewConnection_ [pgSQL| INSERT INTO users (email) VALUES (${email}); |]
processEvent NoteCreated {userId, content} = do
  -- Причины облома: пользователь превысил квоту notes, ...
  let transformError :: PGError -> ProcessingError
      transformError _err = InternalProcessingError
      maxNotesPerUser :: Int64
      maxNotesPerUser = 64
  [Just notesCount] <- withExceptT transformError . ExceptT . try $ do
    runQueryWithNewConnection [pgSQL| SELECT count(note_id) FROM notes WHERE user_id = ${userId}; |] :: IO [Maybe Int64]
  liftIO $ fmtLn $ "notesCount: " +| notesCount |+ ""
  if notesCount < maxNotesPerUser
    then withExceptT transformError . ExceptT . try $ runQueryWithNewConnection_ [pgSQL| INSERT INTO notes (user_id, content) VALUES (${userId}, ${content}); |]
    else throwError . InvalidOperation . fmt $ "Notes limit of " +| maxNotesPerUser |+ " exceeded. Consider upgrading to the Platinum Pro Plus plan to submit up to 4 notes."
processEvent NoteRemoved {userId, noteId} = do
  -- Причины облома: пользователь удаляет не своё
  -- (в таком случае нам нужно прочитать количество affected строк,
  -- при несовпадении владельца оно будет 0 и ничего не изменится,
  -- но ошибку-то надо сообщать), ...
  let transformError :: PGError -> ProcessingError
      transformError _err = InternalProcessingError
  withExceptT transformError . ExceptT . try $
    runQueryWithNewConnection_ [pgSQL| DELETE from notes WHERE user_id = ${userId} AND note_id = ${noteId}; |]
processEvent NoteUpdated {userId, noteId, content} = do
  -- Причины облома: пользователь редактирует не своё
  -- (в таком случае нам нужно прочитать количество affected строк,
  -- при несовпадении владельца оно будет 0 и ничего не изменится,
  -- но ошибку-то надо сообщать), ...
  let transformError :: PGError -> ProcessingError
      transformError _err = InternalProcessingError
  withExceptT transformError . ExceptT . try $
    runQueryWithNewConnection_ [pgSQL| UPDATE notes SET content = ${content} WHERE user_id = ${userId} AND note_id = ${noteId}; |]

preProcessEvent :: Int32 -> UUID.UUID -> Aeson.Value -> Bool -> ExceptT ProcessingError IO ()
preProcessEvent eventId _uuid body isFirst = do
  case Aeson.fromJSON body of
    -- Event body was fine
    Aeson.Success evt -> do
      -- Attempt to process event
      res <- liftIO . runExceptT $ processEvent evt
      liftIO $ print res
      -- Open a brand new DB connection for transaction, so that we mark a failed event as processed anyways
      let markAsProcessed =
            if isFirst
              then [pgSQL| INSERT INTO last_processed_event (event_id) VALUES (${eventId}); |]
              else [pgSQL| UPDATE last_processed_event SET event_id = ${eventId}; |]
      liftIO $ runQueryWithNewConnection_ markAsProcessed

    -- Event body was unexpected
    Aeson.Error _ -> pure ()

processEventsInLoop :: TBChan () -> IO ()
processEventsInLoop chan = do
  newEvents <-
    runQueryWithNewConnection
      [pgSQL| 
    SELECT event_id, uuid, body, is_first
    FROM (
        (SELECT e.event_id, uuid, body, FALSE as is_first
        FROM events e, last_processed_event
        WHERE e.event_id > last_processed_event.event_id AND EXISTS (SELECT 1 FROM last_processed_event)
        ORDER BY e.event_id ASC
        LIMIT 1)
      UNION
        SELECT e.event_id, uuid, body, TRUE as is_first
        FROM events e
        WHERE event_id = 1 AND NOT EXISTS (SELECT 1 FROM last_processed_event )) RESULTS
    ORDER BY event_id DESC
    LIMIT 1;
    |] ::
      IO [(Maybe Int32, Maybe UUID.UUID, Maybe Aeson.Value, Maybe Bool)]

  case newEvents of
    [(Just eventId, Just uuid, Just body, Just isFirst)] -> do
      putStrLn $ "processing event " ++ show (eventId, uuid, isFirst)
      void $ runExceptT $ preProcessEvent eventId uuid body isFirst
    _ -> do
      putStrLn "No unprocessed events found. Waiting for a signal to process new events..."
      atomically $ readTBChan chan
      putStrLn "Got a signal, processing!"
  processEventsInLoop chan
