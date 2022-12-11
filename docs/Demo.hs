
module Demo where 

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

-- | Lexer

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["->", "|", ":=", "(", ")", "$"]
    names = []
    style = emptyDef
      { Tok.commentLine = "--"
      , Tok.commentStart = "--[["
      , Tok.commentEnd = "]]"
      , Tok.identStart = letter
      , Tok.identLetter = alphaNum
      , Tok.reservedOpNames = ops
      , Tok.reservedNames = names
      , Tok.caseSensitive = True
      }

-- | Parser

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

integer :: Parser Integer
integer = Tok.integer lexer

semi :: Parser String
semi = Tok.semi lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

-- | parser

data Expr
  = Var String
  | Abs String Expr
  | App Expr Expr
  | Assign String Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Exp Expr Expr
  | Fact Expr
  | And Expr Expr
  | Or Expr Expr
  | Xor Expr Expr
  | Not Expr
  | Gt Expr Expr
  | Lt Expr Expr
  | Gte Expr Expr
  | Lte Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Int Integer
  deriving (Show)


lambda :: Parser Expr
lambda = do
  reservedOp "("
  var <- identifier
  reservedOp "->"
  expr <- expr
  reservedOp ")"
  return $ Abs var expr

var :: Parser Expr
var = do
  name <- identifier
  return $ Var name

int :: Parser Expr
int = do
  num <- integer
  return $ Int num

parens' :: Parser Expr
parens' = do
  reservedOp "("
  expr <- expr
  reservedOp ")"
  return expr

pipe :: Parser Expr
pipe = do
  reservedOp "("
  expr1 <- expr
  reservedOp "|"
  expr2 <- expr
  reservedOp ")"
  return $ App expr1 expr2

binary :: String -> (Expr -> Expr -> Expr) -> Assoc -> Operator String () Identity Expr
binary name fun = Infix (reservedOp name >> return fun)

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"

table :: [[Operator String () Identity Expr]]
table =
  [ [ binary "_implicit_mul" Mul AssocLeft
    , binary "_implicity_div" Div AssocLeft
    , binary "_implicit_mod" Mod AssocLeft
    ]
  , [ binary "_implicit_add" Add AssocLeft
    , binary "_implicit_sub" Sub AssocLeft
    ]
  , [ binary "_implicit_exp" Exp AssocLeft
    ]
  , [ binary "_implicit_and" And AssocLeft
    , binary "_implicit_or" Or AssocLeft
    , binary "_implicit_xor" Xor AssocLeft
    ]
  , [ binary "_implicit_gt" Gt AssocLeft
    , binary "_implicit_lt" Lt AssocLeft
    , binary "_implicit_gte" Gte AssocLeft
    , binary "_implicit_lte" Lte AssocLeft
    , binary "_implicit_eq" Eq AssocLeft
    , binary "_implicit_neq" Neq AssocLeft
    ]
  ]

term :: Parser Expr
term =  parens' <|> pipe <|> lambda <|> var <|> int <?> "simple expression"


-- | parse string

parseString :: String -> Expr
parseString str = case parse (whiteSpace >> expr) "" str of
  Left e -> error $ show e
  Right r -> r



