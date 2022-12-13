module MrCRepl where

import           Control.Monad (unless)
import           Control.Applicative (liftA)
import           Data.List     (isPrefixOf)
import           Data.Either   (either)
import           GHC.IO.Handle (hFlush)
import           MrCParser     (parseExpr)
import           System.IO     (IO, putStr, putStrLn, stdout)
import           MrCReducer    (initMachine, convertExpr, ruleSet, reduce, Machine (Machine), MachineState)

-- reader :: IO String
-- reader = putStr "MrClean> "
--      >> hFlush stdout
--      >> getLine

-- evaluation :: String -> String
-- evaluation s = (either (const "invalid expression") (either id (f . reduce . initMachine) . convertExpr) $ parseExpr s)
--   where f = either id show

-- stacktrace :: MachineState -> IO ()
-- stacktrace (Right (Machine h c s)) = do putStrLn ("Current heap: " ++ show h)
--                                         putStrLn ("Current stack: " ++ show s)
--                                         putStrLn ("Current expr: " ++ show c)
--                                         putStrLn "Applying rule: ..."
--                                         stacktrace $ ruleSet (Machine h c s)
-- stacktrace (Left (Left s)) = putStrLn ("Failed ruleset with message: " ++ s)
-- stacktrace (Left (Right e)) = putStrLn ("Final expression: " ++ show e)

-- stacktrace_enter :: String -> IO ()
-- stacktrace_enter s = case parseExpr s of
--                       Right e -> case convertExpr e of
--                                     Right e' -> stacktrace $ Right $ initMachine e'
--                                     Left s -> putStrLn ("converting expression failed with: " ++ s)
--                       Left s -> putStrLn ("parsing failed with the message: " ++ show s)

-- printer :: String -> IO ()
-- printer = print . evaluation

-- help :: IO ()
-- help = putStr $ unlines [
--     "Type ':q' to quit."
--  ,  "Type ':h' to get help."
--  ]

-- replEntry :: IO ()
-- replEntry = do
--   help
--   repl []

-- handleOption :: [String] -> String -> IO ()
-- handleOption h ":h" = help >> repl h
-- handleOption _ ":q" = putStr "Goodbye!\n"
-- handleOption h o    = putStr ("invalid option `" ++ o ++ "`\n") >> repl h

-- repl :: [String] -> IO ()
-- repl h = do
--   input <- reader
--   case () of _
--               -- if the input is up arrow or down arrow then we want to use the previous input
--               | (not $ null h) && input == "\ESC[A" || input == "\ESC[B" -> stacktrace_enter (h !! 0) >> repl h
--               | ":" `isPrefixOf` input -> handleOption h input
--               | otherwise -> stacktrace_enter input >> repl (input : h)

-- import          Control.Monad.IO.Class (liftIO)
-- import          System.Console.Haskeline

-- import Control.Monad.Trans
-- import Data.List (isPrefixOf)
-- import System.Console.Repline


-- type Repl a = HaskelineT IO a

-- -- Evaluation : handle each line user inputs
-- cmd :: String -> Repl ()
-- cmd input = liftIO $ print input

-- -- Tab Completion: return a completion for partial words entered
-- completer :: Monad m => WordCompleter m
-- completer n = do
--   let names = ["kirk", "spock", "mccoy"]
--   return $ filter (isPrefixOf n) names

-- -- Commands
-- help :: [String] -> Repl ()
-- help args = liftIO $ print $ "Help: " ++ show args

-- say :: [String] -> Repl ()
-- say args = do
--   _ <- liftIO $ system $ "cowsay" ++ " " ++ (unwords args)
--   return ()

-- opts :: [(String, [String] -> Repl ())]
-- opts = [
--     ("help", help)  -- :help
--   , ("say", say)    -- :say
--   ]

-- ini :: Repl ()
-- ini = liftIO $ putStrLn "Welcome!"

-- repl :: IO ()
-- repl = evalRepl (pure ">>> ") cmd opts Nothing (Word completer) ini



import Control.Monad.Trans
import Data.List (isPrefixOf)
import System.Console.Repline
import System.Process (callCommand)

type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

say :: String -> Repl ()
say args = do
  _ <- liftIO $ callCommand $ "cowsay" ++ " " ++ args
  return ()

opts :: [(String, String -> Repl ())]
opts =
  [ ("help", help . words), -- :help
    ("say", say) -- :say
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

repl_alt :: IO ()
repl_alt = evalReplOpts $ ReplOpts
  { banner           = const $ pure ">>> "
  , command          = cmd
  , options          = opts
  , prefix           = Just ':'
  , multilineCommand = Just "paste"
  , tabComplete      = (Word0 completer)
  , initialiser      = ini
  , finaliser        = final
  }

customBanner :: MultiLine -> Repl String
customBanner SingleLine = pure ">>> "
customBanner MultiLine = pure "| "

repl :: IO ()
repl = evalRepl (const $ pure ">>> ") cmd opts (Just ':') (Just "paste") (Word0 completer) ini final

main :: IO ()
main = pure ()
