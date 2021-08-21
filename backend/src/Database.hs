{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.PostgreSQL.Typed.Protocol
import Database.PostgreSQL.Typed (PGError, pgConnect, pgDisconnect, pgErrorFields)
import Database.PostgreSQL.Typed.Query (PGSimpleQuery, pgQuery)
import Database.PostgreSQL.Typed.TH (getTPGDatabase)
import qualified Network.Socket as Net
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Lazy as Map
import System.Environment (getEnv)
import Control.Monad (void)

includesText :: BSC.ByteString -> PGError -> Bool
includesText subStr = BSC.isInfixOf subStr . extractMessage
  where
    extractMessage :: PGError -> BSC.ByteString
    extractMessage = Map.findWithDefault "" 'M' . pgErrorFields

runQueryWithNewConnection :: PGSimpleQuery a -> IO [a]
runQueryWithNewConnection q = do
  db <- getTPGDatabase
  conn <- pgConnect db
  res <- pgQuery conn q
  pgDisconnect conn
  pure res

runQueryWithNewConnection_ :: PGSimpleQuery a -> IO ()
runQueryWithNewConnection_ = void . runQueryWithNewConnection