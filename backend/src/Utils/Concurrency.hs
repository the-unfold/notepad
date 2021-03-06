{-# LANGUAGE Safe #-}

module Utils.Concurrency (waitForChildren, forkChild) where

import Control.Concurrent (ThreadId, forkFinally)
import safe Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

waitForChildren :: MVar [MVar ()] -> IO ()
waitForChildren children = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m : ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren children

forkChild :: MVar [MVar ()] -> IO () -> IO ThreadId
forkChild children io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar : childs)
  forkFinally io (\_ -> putMVar mvar ())