{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.PostgreSQL.Typed.Protocol
import Database.PostgreSQL.Typed (PGError, pgConnect, pgDisconnect, pgErrorFields, useTPGDatabase)
import Database.PostgreSQL.Typed.Query (PGSimpleQuery, pgQuery, pgSQL, pgExecute)
import Database.PostgreSQL.Typed.Types (PGType)
import qualified Network.Socket as Net
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Lazy as Map

db :: PGDatabase
db =
  PGDatabase
    { pgDBAddr = Right $ Net.SockAddrInet 5431 (Net.tupleToHostAddress (127, 0, 0, 1)),
      pgDBName = "notepad",
      pgDBUser = "admin",
      pgDBPass = "admin",
      pgDBParams = [],
      pgDBDebug = False,
      pgDBLogMessage = print . PGError,
      pgDBTLS = TlsDisabled
    }

includesText :: BSC.ByteString -> PGError -> Bool
includesText subStr = BSC.isInfixOf subStr . extractMessage
  where
    extractMessage :: PGError -> BSC.ByteString
    extractMessage = Map.findWithDefault "" 'M' . pgErrorFields

runQueryNoTransaction :: PGSimpleQuery a -> IO [a]
runQueryNoTransaction q = do
  conn <- pgConnect db
  res <- pgQuery conn q
  pgDisconnect conn
  pure res

runQueryNoTransaction_ :: PGSimpleQuery a -> IO ()
runQueryNoTransaction_ q = do
  conn <- pgConnect db
  pgQuery conn q
  pgDisconnect conn
  pure ()

pgQuery_ :: PGConnection -> PGSimpleQuery a -> IO ()
pgQuery_ conn q = do
  pgQuery conn q
  pure ()