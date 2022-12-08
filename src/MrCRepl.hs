module MrCRepl where

import           Control.Monad (unless)
import           Data.List     (isPrefixOf)
import           GHC.IO.Handle (hFlush)
import           MrCParser     (parseExpr)
import           System.IO     (IO, putStr, putStrLn, stdout)

reader :: IO String
reader = putStr "MrClean> "
     >> hFlush stdout
     >> getLine

evaluation :: String -> String
evaluation input = input

printer :: String -> IO ()
printer = print . parseExpr

help :: IO ()
help = putStr $ unlines [
    "Type ':q' to quit."
 ,  "Type ':h' to get help."
 ]

replEntry :: IO ()
replEntry = do
  help
  repl

handleOption :: String -> IO ()
handleOption ":h" = help >> repl
handleOption ":q" = putStr "Goodbye!\n"
handleOption o    = putStr ("invalid option `" ++ o ++ "`\n") >> repl

repl :: IO ()
repl = do
  input <- reader
  case () of _
              | ":" `isPrefixOf` input -> handleOption input
              | otherwise -> printer (evaluation input) >> repl
