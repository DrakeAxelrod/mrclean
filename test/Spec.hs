-- import Lib
-- 

import MrCParser (
  regularParse,
  completeParse,
  parseForTest,
  )


import Common (Expr (..))

-- import           Control.Applicative (many, (*>), (<$), (<$>), (<*), (<*>),
--                                       (<|>))
-- import           Control.Monad       (ap, void)
-- import           Data.Char           (isSpace)
-- import           Text.Parsec         (chainl1, eof, many1, parse, (<?>), try)
-- import           Text.Parsec.Char    (char, digit, letter, oneOf, satisfy,
--                                       spaces)
import           Text.Parsec.Error   (ParseError)
-- import           Text.Parsec.String  (Parser)


main :: IO ()
main = parseTestFileAndStrip >>= print

-- | read in test file ./assets/test.mrc
-- | and return the contents as a String
readTestFile :: IO String
readTestFile = readFile "./assets/test.mrc"

-- | parse the test file
parseTestFile :: IO (Either ParseError Expr)
parseTestFile = do completeParse parseForTest <$> readTestFile
-- parseTestFile = do completeParse parseFunc <$> readTestFile
stripExpr :: Expr -> String
stripExpr (ExprVar x) = x
stripExpr (ExprApp lhs rhs) = "(" ++ stripExpr lhs ++ "|" ++ stripExpr rhs ++ ")"

parseTestFileAndStrip :: IO (Either ParseError String)
parseTestFileAndStrip = do
  (fmap stripExpr) <$> parseTestFile
