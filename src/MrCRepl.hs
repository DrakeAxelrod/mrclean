module MrCRepl where

import           Control.Monad (unless)
import           Control.Applicative (liftA)
import           Data.List     (isPrefixOf)
import           Data.Either   (either)
-- import           GHC.IO.Handle (hFlush)
import           MrCParser     (parseExpressions)
import           System.IO     (IO, putStr, putStrLn, stdout)
import           MrCReducer    (initMachine, convertExpr, runMany, reduce, Machine (Machine), MachineState)
import           Control.Monad.Trans
import           Data.List (isPrefixOf)
import           System.Console.Repline
import           System.Process (callCommand)

-- evaluation :: VarHeap -> String -> (String, VarHeap)
-- evaluation h s = 
--   (either r (either g (f . reduceFull . (\e -> Machine h e [])) . convertExpr) $ parseExpr s)
--     where f (Left (Right (Machine h' e _))) = (show e, h')
--           f (Left (Left e))                 = (e, h)
--           f (Right _)                       = undefined -- this will never happen
--           g x                               = (x, h)
--           r e                               = (show e, h)

evaluation' = show . parseExpressions

evaluation :: String -> String
evaluation s = either show f $ parseExpressions s
  where f e = either id (g . runMany) $ sequenceA $ map convertExpr e
        g (Left (Right (Machine _ e _))) = show e
        g (Left (Left s)) = s
        g (Right _) = "something went wrong..."

-- stacktrace :: MachineState -> IO ()
-- stacktrace (Right (Machine h c s)) = do putStrLn ("Current heap: " ++ show h)
--                                         putStrLn ("Current stack: " ++ show s)
--                                         putStrLn ("Current expr: " ++ show c)
--                                         putStrLn "Applying rule: ..."
--                                         stacktrace $ ruleSet (Machine h c s)
-- stacktrace (Left (Left s)) = putStrLn ("Failed ruleset with message: " ++ s)
-- stacktrace (Left (Right e)) = putStrLn ("Final expression: " ++ show e)
-- 
-- stacktrace_enter :: String -> IO ()
-- stacktrace_enter s = case parseExpr s of
--                       Right e -> case convertExpr e of
--                                     Right e' -> stacktrace $ Right $ initMachine e'
--                                     Left s -> putStrLn ("converting expression failed with: " ++ s)
--                       Left s -> putStrLn ("parsing failed with the message: " ++ show s)

type Repl a = HaskelineT IO a

prompt :: String
prompt = "MrClean> "

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ putStrLn . evaluation $ input

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = [":help", ":clear", ":multi", ":quit"]
  return $ filter (isPrefixOf n) names

helpMsg :: String
helpMsg = "\
\Type an expression to evaluate it.\n\
\Type :help for help.\n\
\Type :clear to clear the screen.\n\
\Type :multi to enter multi-line mode.\n\
\Type :quit or ctrl-d to exit"

-- Commands
help :: Repl ()
help = liftIO $ putStrLn helpMsg

clear :: Repl ()
clear = liftIO $ callCommand "clear"

quit :: Repl ()
quit = do
  liftIO $ putStrLn "Goodbye!"
  abort

opts :: [(String, String -> Repl ())]
opts =
  [ ("help", const help) -- :help
  , ("clear", const clear) -- :clear
  , ("quit", const quit) -- :quit
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to the MrClean REPL! you can type :help for help."

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

-- repl_alt :: IO ()
-- repl_alt = evalReplOpts $ ReplOpts
--   { banner           = customBanner
--   , command          = cmd
--   , options          = opts
--   , prefix           = Just ':'
--   , multilineCommand = Just "multi"
--   , tabComplete      = (Word0 completer)
--   , initialiser      = ini
--   , finaliser        = final
--   }

customBanner :: MultiLine -> Repl String
customBanner SingleLine = pure $ prompt
customBanner MultiLine = pure $ "| "

repl :: IO ()
repl = evalRepl customBanner cmd opts (Just ':') (Just "multi") (Word0 completer) ini final
