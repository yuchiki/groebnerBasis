module Lib
    ( someFunc, helloWorld
    ) where

-- |
-- >>> helloWorld "Hay!"
-- "Hay! World!"
helloWorld :: String -> String
helloWorld = (++ " World!")

someFunc :: IO ()
someFunc = putStrLn $ helloWorld "Hello"
