module MrClean where


import qualified MrCRepl
import           Options.Applicative

data Options = Options
  { optStr  :: String
  , optFile :: FilePath
  , authors :: Bool
  }


options :: Parser Options
options = Options
      <$> strOption
          ( long "string"
         <> short 's'
         <> help "Evaluate the given string"
         <> value ""
         <> metavar "STRING")
      <*> strOption
          ( long "file"
         <> short 'f'
         <> help "Evaluate the given file"
         <> value ""
         <> metavar "FILE")
      <*> switch
          ( long "authors"
         <> short 'a'
         <> help "Print the authors of this project")



cli :: IO ()
cli = do
  mrcBanner
  run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
      <> progDesc "If no options are given, the REPL is started."
      <> header "The cli for MrClean"
      )

run :: Options -> IO ()
run (Options "" "" False) = MrCRepl.replEntry
run (Options s "" False)  = putStr s
run (Options "" f False)  = putStr f
run (Options _ _ True)    = putStr $ unlines [
    "MrClean authors:"
  , "Drake Axelrod (drake@draxel.io)"
  , "Hugo Lom (lohugo@chalmers.se)"
  ]
run (Options _ _ False)   = putStr $ unlines [
    "MrClean error: cannot parse both string and file at the same time"
  , ""
  , "EXAMPLE:"
  , "\tmrclean --string \"x := x -> x|x\""
  , "\tmrclean --file \"file.mrc\""
  , ""
  , "Try 'mrclean --help' for more information."
  ]


mrcBanner :: IO ()
mrcBanner = putStr " \n\
\\x1b[30m            .~!!:                                   ^!~.                                              \n\
\\x1b[31m           !GP5PG5.                    .:~~!!!~^:  5P5PG.                                             \n\
\\x1b[32m  .^~^.    ?P55555G7                 :7777777777777P555B:                        .                    \n\
\\x1b[33m ?PPPPPJ.  :G555555P5:    ~JJ?~!77:.7?7!7??!~~!77?YP555G.  .......       .~7??!?55P?    ~777~^~~^.    \n\
\\x1b[34m 5P5555PG?  P555GP555G7  .GYY5555PGJ7!!7J^      ..!G55PP .::::^:::::.  .JP555PP5YYYG~  ^?!!!7??777?~  \n\
\\x1b[35m :G555555PP!YP55G?G555G5.:GYYY5PY?7?!!!J~         !G55P5:^::^~::^::::^.55YYPY!!55YY5P  ?7!!?7::77!!J^ \n\
\\x1b[36m  YP555GP55PP555B^.PP55PPYPYY5P.  .J!!!J:         JP55GY^:::^:::::^^^~!PYY5G    PYYYG^:?!!7J   77!!J~ \n\
\\x1b[37m  :G555B7JG5Y555GY  JGP5GGYYYPJ    !?!!77.        5555B!::::::^^^^~^::~GYYYG^  .PYYY5P?7!!J~  .?!!7Y. \n\
\\x1b[90m   5P55PP ^5P5555B.  :7?!P5YYG!     !?7!777!!!77!^P555B^ ^::::^::::::^~!P5YY5YJ55Y5YY57!!?Y   77!!?7  \n\
\\x1b[91m   .G555G7  ~PPPG5       7P5PG:      .!77777777?J75PPGG.  .:^::::^^~^:  .?555555PPJYY?~77?:  .J777Y.  \n\
\\x1b[92m    !G55PB.   .:.         .^^.          .:^~~~~^.  :~^.      ..::..        :~!!~:             :!77:   \n\
\\x1b[92m     ^Y5Y!                                                                                            \x1b[m\n\
\ >> MrClean - A functional programming language\n\n"
