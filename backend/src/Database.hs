module Database where

import Database.PostgreSQL.Typed (PGError, pgConnect, pgDisconnect, pgErrorFields)
import Database.PostgreSQL.Typed.Query (PGSimpleQuery, pgQuery)
import Database.PostgreSQL.Typed.TH (getTPGDatabase)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Lazy as Map
import Control.Monad (void)

{-|
The `postgresql-typed` looks for the following ENV 
variables both in compile time and in the runtime:
TPG_DB TPG_USER TPG_PASS TPG_HOST TPG_PORT 

They should be specified in .env file 
and used with the specified command from README.md
Otherwise, `postgresql-typed` will try to use default settings, 
which we don't want to use (access rights, etc).
-}

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