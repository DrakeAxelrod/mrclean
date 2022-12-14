{- Lab 4
   Date: 2022-12-14
   Authors: Hugo Lom, Drake Axelrod
   Lab group: 68
 -}
module MrCRepl where

import           Data.List     (isPrefixOf)
import           MrCParser     (parseExpressions, printExpr)
import           MrCReducer    (convertExpr, runMany, Machine (..), convertExprBack)
import           Control.Monad.Trans
import           System.Console.Repline
import           System.Process (callCommand)

-- | evaluation of a string to a value
evaluation :: String -> String
evaluation s = either show f $ parseExpressions s
  where f e = either id (g . runMany) $ traverse convertExpr e
        g (Left (Right (Machine _ e _))) = printExpr $ convertExprBack e
        g (Left (Left e)) = e
        g (Right _) = "something went wrong..."

-- | Repl Monad
type Repl a = HaskelineT IO a

-- | Repl Prompt text
prompt :: String
prompt = "MrClean> "

-- | Evaluate the user input and output send the result to  
cmd :: String -> Repl ()
cmd input = liftIO $ putStrLn . evaluation $ input

-- | Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = [":help", ":clear", ":multi", ":quit"]
  return $ filter (isPrefixOf n) names

-- | Help message
helpMsg :: String
helpMsg = "\
\Type an expression to evaluate it.\n\
\Type :help for help.\n\
\Type :clear to clear the screen.\n\
\Type :multi to enter multi-line mode.\n\
\Type :quit or ctrl-d to exit"

-- | Help command
help :: Repl ()
help = liftIO $ putStrLn helpMsg

-- | Clear command
clear :: Repl ()
clear = liftIO $ callCommand "clear"

-- | Quit command
quit :: Repl ()
quit = do
  liftIO $ putStrLn "Goodbye!"
  abort

-- | Bind the names to the actions
opts :: [(String, String -> Repl ())]
opts =
  [ ("help", const help)
  , ("clear", const clear)
  , ("quit", const quit)
  ]

-- | Entry point
ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to the MrClean REPL! you can type :help for help."

-- | Exit point
final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

-- | Custom banner to handle multi-line mode
customBanner :: MultiLine -> Repl String
customBanner SingleLine = pure prompt
customBanner MultiLine = pure "| "

-- | Run the repl
repl :: IO ()
repl = evalRepl customBanner cmd opts (Just ':') (Just "multi") (Word0 completer) ini final
