module Lib
  ( someFunc,
  )
where

-- useTPGDatabase db -- compile time connection

someFunc :: IO ()
someFunc = putStrLn "someFunc"
