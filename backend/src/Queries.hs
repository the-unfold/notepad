{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Queries where
-- This module contains only read queries, no mutations and no event-related stuff

import Database.PostgreSQL.Typed (pgSQL)
import Database (runQueryWithNewConnection)
import HttpTypes (Note (Note))
import Data.Int (Int32)

-- TODO: ExceptT
queryNotes :: Int32 -> IO [Note]
queryNotes userId = do
  fmap (uncurry Note) <$> runQueryWithNewConnection [pgSQL| SELECT note_id, content from notes WHERE user_id = ${userId} ORDER BY note_id ASC; |]

-- TODO: ExceptT
queryNote :: Int32 ->  Int32 -> IO Note
queryNote noteId userId = do
  [note] <- fmap (Note noteId) <$> runQueryWithNewConnection [pgSQL| SELECT content from notes WHERE user_id = ${userId} AND note_id = ${noteId}; |]
  pure note
