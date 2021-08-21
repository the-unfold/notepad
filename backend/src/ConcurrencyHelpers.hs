module ConcurrencyHelpers where

import Control.Concurrent.MVar
import Control.Concurrent (ThreadId, forkFinally)

waitForChildren :: MVar [MVar ()] -> IO ()
waitForChildren children = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren children

forkChild :: MVar [MVar ()] -> IO () -> IO ThreadId
forkChild children io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkFinally io (\_ -> putMVar mvar ())