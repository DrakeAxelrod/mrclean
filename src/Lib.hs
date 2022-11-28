
module Lib
    ( run
    ) where

import           Text.Parsec


run :: IO ()
run = print $ show $ rhsPipeParse "foo | bar"
-- parse (many (char 'c') >> eof) "" "cccccc"



-- extract the right hand side of a '|' expression (e.g. "foo | bar")
rhsPipeParse :: String -> Either ParseError String
rhsPipeParse = parse (many (noneOf "|") >> char '|' >> many (noneOf "|") >> eof) ""


data Expr = Number Integer | Variable String | Apply Expr Expr | Assign String Expr

-- yoink https://jakewheat.github.io/intro_to_parsing/#very-simple-expression-parsing
variable :: Parser String
variable = do
    fc <- firstChar
    rest <- many nonFirstChar
    return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')


-- In do-notation

-- a >> b >> c >> d

-- is equivalent to

-- do a
--    b
--    c
--    d

--  parseShow :: String -> String
--  parseShow = run_parser showParser

--  showParser :: Parser String

--  run_parser :: Parser a -> String -> a
--  run_parser p str = case parse p "" str of
--  Left err -> error $ "parse error at " ++ (show err)
--  Right val -> val
