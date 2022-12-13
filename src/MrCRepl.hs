module MrCRepl where

import           Control.Monad (unless)
import           Control.Applicative (liftA)
import           Data.List     (isPrefixOf)
import           Data.Either   (either)
import           GHC.IO.Handle (hFlush)
import           MrCParser     (parseExpr)
import           System.IO     (IO, putStr, putStrLn, stdout)
import           MrCReducer    (initMachine, convertExpr, ruleSet, reduce, Machine (Machine), MachineState)

reader :: IO String
reader = putStr "MrClean> "
     >> hFlush stdout
     >> getLine

evaluation :: String -> String
evaluation s = e ++ " " ++ (either (const "invalid expression") (either id (reduce . initMachine) . convertExpr) $ parseExpr s)
  where e = either (const "") show $ parseExpr s

stacktrace :: MachineState -> IO ()
stacktrace (Right (Machine h c s)) = do putStrLn ("Current heap: " ++ show h)
                                        putStrLn ("Current stack: " ++ show s)
                                        putStrLn ("Current expr: " ++ show c)
                                        putStrLn "Applying rule: ..."
                                        stacktrace $ ruleSet (Right (Machine h c s))
stacktrace (Left s) = putStrLn ("Failed ruleset with message: " ++ s)

stacktrace_enter :: String -> IO ()
stacktrace_enter s = case parseExpr s of
                      Right e -> case convertExpr e of
                                    Right e' -> stacktrace $ initMachine e'
                                    Left s -> putStrLn ("converting expression failed with: " ++ s)
                      Left s -> putStrLn ("parsing failed with the message: " ++ show s)

printer :: String -> IO ()
printer = print . evaluation

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
              | otherwise -> stacktrace_enter input >> repl
