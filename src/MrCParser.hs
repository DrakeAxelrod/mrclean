module MrCParser where 

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
import Data.Functor.Identity (Identity)

{- ===== Language Explanation =====
  Functional language that mimics lambda calculus
  Variables are alphanumerical strings (eg x, x1, my_var)
  Abstraction (or functions) are written as (x -> M), where x is a bound variable and M is an expressions
  Applications are written as (N|M) where N is applied to the function M
  
  Parenthesis are used to group expressions e.g: (x -> (x|x)) is a valid expression
  You can assign to variables with the following syntax (x := M), which means that M is assigned to x
  reserved operators: ->, |, :=, (, ), $, &
  
  Examples:
  assign x to the identity function: x := (x -> x)
  assign y to the function that takes a function and applies it to itself: y := (f -> (f|f))
  assign z to the function that takes a function and applies it to itself twice: z := (f -> ((f|f)|f))
  etc...
  1 + 2 * 3 = 7
  1 + (2 * 3) = 7
  (1 + 2) * 3 = 9

  $ reduces the expression to its beta normal form
  -> is used to define functions or lambdas (eg: x -> x)
  | is used to apply functions (eg: (x|x))
  := is used to assign a value to a variable
  arithmetic operators: 
    _implicit_addition: 1 2 = 3
    _implicit_multiplication: 3 2 = 6
    _implicit_subtraction: 3 2 = 1
    _implicit_division: 6 2 = 3
    _implicit_modulo: 7 2 = 1
    _implicit_exponentiation: 2 3 = 8
    _implicit_factorial: 5! = 120
    _implicit_and: 1 1 = 1
    _implicit_or: 1 0 = 1
    _implicit_xor: 1 0 = 1
    _implicit_not: 1 = 0
    _implicit_greater_than: 2 1 = 1
    _implicit_less_than: 1 2 = 1
    _implicit_greater_than_or_equal_to: 2 2 = 1
    _implicit_less_than_or_equal_to: 2 2 = 1
    _implicit_equal_to: 2 2 = 1
    _implicit_not_equal_to: 2 1 = 1
-}

data Expr = Var String
          | Application Expr Expr
          | Lambda Expr Expr
          | Assign Expr Expr
          deriving (Show)

mrCleanDef :: Tok.LanguageDef st
mrCleanDef = emptyDef
          { Tok.commentStart    = "--[["
          , Tok.commentEnd      = "]]"
          , Tok.commentLine     = "--"
          , Tok.nestedComments  = True
          , Tok.identStart      = noneOf ["\n", "\t", "\r", "->", "|", ":="]
            -- noneOf "|\n \t\r"
          , Tok.identLetter     = noneOf "|\n \t\r"
          -- , opStart         = opLetter lexer
          -- , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
          , Tok.reservedOpNames = ["|" , "->", ":="]
          , Tok.reservedNames   = []
          , Tok.caseSensitive   = False
          }

lexer :: Tok.TokenParser st
lexer = Tok.makeTokenParser mrCleanDef

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

pipe :: Parser Expr
pipe = do
  e1 <- expr
  reservedOp "|"
  e2 <- expr
  return $ Application e1 e2

assign :: Parser Expr
assign = do
  var <- expr
  reservedOp ":="
  e <- expr
  return $ Assign var e

lambda :: Parser Expr
lambda = do
  var <- expr
  reservedOp "->"
  e <- expr
  return $ Lambda var e

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"

table :: [[Operator String () Identity Expr]]
table = [ [Prefix (reservedOp "$" >> return (Application (Var "reduce")))]
        , [Infix (reservedOp "|" >> return Application) AssocLeft]
        , [Infix (reservedOp ":=" >> return Assign) AssocLeft]
        , [Infix (reservedOp "->" >> return Lambda) AssocLeft]
        ]

term :: Parser Expr
term =  parens expr
    <|> Var <$> identifier
    <?> "term"

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whiteSpace >> expr) "<stdin>"


