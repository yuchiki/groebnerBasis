module Lib
    ( repl, helloWorld
    ) where

import           Control.Monad
import qualified Exp
                 (Exp, parse, (***), (+++))
import           System.IO
import           Util

helloWorld :: String -> String
helloWorld = (++ " World!")

repl :: IO ()
repl = do
    putStr ">" ; hFlush stdout
    str <- getLine
    when (str `elem` ["q", "Q", "quit", "Quit", "QUIT"]) (return ())
    case Exp.parse str of
         Left e -> putStrLn e
         Right e -> printLn e
    repl
