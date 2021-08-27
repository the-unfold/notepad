{-# LANGUAGE DataKinds #-}

module HttpServer (handleRequests) where

import HttpTypes
import Control.Concurrent.STM.TBChan (TBChan)
import Control.Monad.Trans (MonadIO (liftIO))
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Application, Get, JSON, Proxy (Proxy), Server, serve, (:>))

handleRequests ::  TBChan () -> IO ()
handleRequests chan = do
  let port = (8000 :: Int)
  putStrLn $ "Starting server at port " <> show port
  run port app


app :: Application
app =
  logStdoutDev
    . cors (const $ Just corsPolicy)
    $ serve notepadApi server
  where
    -- Note: Content-Type header is necessary for POST requests
    corsPolicy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}

notepadApi :: Proxy NotepadApi
notepadApi = Proxy

type NotepadApi =
  "hello" :> Get '[JSON] Int

server :: Server NotepadApi
server =
  hello
  where
    hello = pure 42
