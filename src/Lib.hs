module Lib
    ( repl, helloWorld
    ) where

import           Control.Monad
import qualified Exp
                 (Exp, parse, (***), (+++))

helloWorld :: String -> String
helloWorld = (++ " World!")

repl :: IO ()
repl = do
    putStrLn ">"
    str <- getLine
    when (str `elem` ["q", "Q", "quit", "Quit", "QUIT"]) (return ())
    case Exp.parse str of
         Left e -> do
            putStrLn e
            repl
         Right e -> do
            print e
            putStrLn ""
            repl
