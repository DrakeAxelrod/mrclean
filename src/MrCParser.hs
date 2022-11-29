module MrCParser (regularParse, completeParse, parseExpr) where

import           Control.Applicative (many, (*>), (<$), (<$>), (<*), (<*>),
                                      (<|>))
import           Control.Monad       (ap, void)
import           Data.Char           (isSpace)
import           Text.Parsec         (chainl1, eof, many1, parse, (<?>), try, between)
import           Text.Parsec.Char    (char, digit, letter, oneOf, satisfy,
                                      space, spaces, string, noneOf)
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

parseSymbol :: String -> Parser String
parseSymbol = lexeme . string

parseVariableS :: Parser String
parseVariableS = lexeme $ many1 $ satisfy isVarChar

isVarChar :: Char -> Bool
isVarChar c = not (isSpace c) && c /= '|' && c /= '(' && c /= ')'

parseVariable :: Parser Expr
parseVariable = ExprVar <$> parseVariableS

-- | added to make the parser more robust
parsePipe :: Parser Char
parsePipe = lexeme $ char '|'

parseParen :: Parser Expr
parseParen = between (lexeme $ char '(')  (lexeme $ char ')') parseExpr

parseTerm :: Parser Expr
-- ^ Parse only expressions on either side of an application
parseTerm = parseParen <|> parseVariable

parseNonParen :: Parser Expr
-- ^ Parse only expressions that don't begin with parenthesis
parseNonParen = try parseApp <|> parseVariable

parseExpr :: Parser Expr
-- ^ Parse an entire expression
parseExpr = try parseApp <|> parseTerm

parseApp :: Parser Expr
parseApp = lexeme $ chainl1 parseTerm op
  where
    op = do
        void parsePipe
        return ExprApp

parseSep :: Parser a -> Parser b -> Parser c -> (b -> c -> d) -> Parser d
-- ^ `parseSep sep lhs rhs cstr` creates a parser that tries to parse `lhs` and `rhs`
-- seperated by `sep` and if it succeds constructs a new value using `cstr`
parseSep sep lhs rhs cstr = 
  do l <- lhs
     void sep
     r <- rhs
     return $ cstr l r

parseAssign :: Parser Expr
parseAssign = parseSep (parseSymbol ":=") parseVariableS parseExpr ExprAssign

parseFunc :: Parser Expr
parseFunc = parseSep (parseSymbol "->") parseVariableS parseExpr ExprFun