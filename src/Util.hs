module Util (printLn) where

printLn :: Show a => a -> IO ()
printLn a = do
    print a
    putStrLn ""
