module MrCParser (regularParse, completeParse, parseExpr) where

import           Control.Applicative (many, (*>), (<$), (<$>), (<*), (<*>),
                                      (<|>))
import           Control.Monad       (ap, void)
import           Data.Char           (isSpace)
import           Text.Parsec         (chainl1, eof, many1, parse, (<?>), try)
import           Text.Parsec.Char    (char, digit, letter, oneOf, satisfy,
                                      spaces)
import           Text.Parsec.Error   (ParseError)
import           Text.Parsec.String  (Parser)

import           Common              (Expr (..))

-- https://github.com/JakeWheat/intro_to_parsing/blob/master/FunctionsAndTypesForParsing.lhs
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

completeParse :: Parser a -> String -> Either ParseError a
completeParse = regularParse . parseEof . parseStart

parseEof :: Parser a -> Parser a
parseEof p = p <* eof

parseStart :: Parser a -> Parser a
parseStart = (spaces *>)

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

parseVariableS :: Parser String
parseVariableS = lexeme $ many1 $ satisfy (\c -> not (isSpace c) && c /= '|')

parseVariable :: Parser Expr
parseVariable = ExprVar <$> parseVariableS

parseApp :: Parser Expr
parseApp = lexeme $ chainl1 parseVariable op
  where
    op = do
        void parsePipe
        return ExprApp

parseExpr :: Parser Expr
parseExpr = try parseApp <|> parseVariable

-- | added to make the parser more robust
parsePipe :: Parser Char
parsePipe = lexeme $ char '|'

parseParen :: Parser Expr
parseParen = (lexeme $ char '(') *> parseExpr <* (lexeme $ char ')')


