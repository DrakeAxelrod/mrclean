{- Lab 4
   Date: 2022-12-14
   Authors: Hugo Lom, Drake Axelrod
   Lab group: 68
 -}
module MrCParser (Expr (..), parseExpr, parseExpressions, parseFile, printExpr) where

import           Data.Functor.Identity (Identity)
import           Text.Parsec           (ParseError, eof, noneOf, oneOf,
                                        parse, (<?>), (<|>))
import           Data.Char             (isAlphaNum, isAscii)
import           Data.Either           (isRight)
import           Text.Parsec.Expr      (Assoc (AssocLeft, AssocRight),
                                        Operator (Infix),
                                        buildExpressionParser)
import           Text.Parsec.Language  (emptyDef)
import           Text.Parsec.Prim      ()
import           Text.Parsec.String    (Parser)
import           Text.Parsec.Combinator (endBy)
import qualified Text.Parsec.Token     as Tok
import           Text.Parsec.Char      (char)

import           Test.QuickCheck       (Gen, Arbitrary, arbitrary, listOf1, oneof, sized)
import           Control.Monad         (liftM2)

-- | The expressions data type
data Expr = Var String
          | Application Expr Expr
          | Lambda Expr Expr
          | Assign Expr Expr
          deriving (Show, Eq)

-- | Check if the given character is a valid identifier character
isValidIdent :: Char -> Bool
isValidIdent c = isAlphaNum c && isAscii c

-- | Generate a valid identifier character
validChar :: Gen Char
validChar = validChar' arbitrary
  where validChar' gc = do c <- gc
                           if isValidIdent c then 
                            return c
                           else 
                            validChar' arbitrary

-- | Generate a valid identifier
validIdent :: Gen String
validIdent = listOf1 validChar

-- | Generate an arbitrary expression, with a bounded depth
genExpr :: Gen Expr
genExpr = sized genExpr'

-- | Arbitrary instance of expression
instance Arbitrary Expr where
  arbitrary = genExpr

-- | Generate a random expression keeping track
-- of the recursive depth and terminating once the
-- depth is reached.
genExpr' :: Int -> Gen Expr
genExpr' n | 0 >= n     = fmap Var validIdent
           | otherwise  = oneof [ fmap Var validIdent
                            , liftM2 Application (fmap Var validIdent) subexpr
                            , liftM2 Lambda (fmap Var validIdent) subexpr
                            , liftM2 Assign (fmap Var validIdent) subexpr
                            ]
  where subexpr = genExpr' (n `div` 2)

-- | Test whether the parser is able to parse the given string.
prop_parseExprValid :: Expr -> Bool
prop_parseExprValid = isRight . parseExpr . printExpr

-- | The text representation for an expression tree.
printExpr :: Expr -> String
printExpr (Var s) = s
printExpr (Application lhs rhs) = "( " ++ printExpr lhs ++ " | " ++ printExpr rhs ++ " )"
printExpr (Lambda arg body) = "( " ++ printExpr arg ++ " -> " ++ printExpr body ++ " )"
printExpr (Assign var body) = "( " ++ printExpr var ++ " := " ++ printExpr body ++ " )"

-- | MrClean Language Definition
--   * Defines all of the language features using the Text.Parsec.Token module.
--   * We use the emptyDef as a base, and overwrite the features we want to change.
mrCleanDef :: Tok.LanguageDef st
mrCleanDef = emptyDef
          {
            Tok.commentStart    = "/*"                     -- ^ start of a block comment.
          , Tok.commentEnd      = "*/"                     -- ^ end of a block comment.
          , Tok.commentLine     = "//"                     -- ^ line comment.
          , Tok.nestedComments  = True                     -- ^ the language supports nested block comments.
          , Tok.identStart      = noneOf "\n \t\r\f|();"   -- ^ don't allow any of these characters to start an identifier.
          , Tok.identLetter     = noneOf "\n \t\r\f|();"   -- ^ don't allow any of these characters to be part of an identifier.
          , Tok.opStart         = oneOf ""                 -- ^ not used in this language.
          , Tok.opLetter        = oneOf ""                 -- ^ not used in this language.
          , Tok.reservedOpNames = ["|" , "->", ":="]  -- ^ reserved operators.
          , Tok.reservedNames   = ["|" , "->", ":="]  -- ^ reserved names.
          , Tok.caseSensitive   = False                    -- ^ case insensitive.
          }

-- | Generated collection of parsing-functions that adhere to the mrCleanDef.
-- The function makeTokenParser generates standard
-- utility parsers that implement the language definition
lexer :: Tok.TokenParser st
lexer = Tok.makeTokenParser mrCleanDef

-- | Parser for identifiers (variables, function-names) which asserts
-- that the identifier does not conflict with the reserved names.
identifier :: Parser String
identifier = Tok.identifier lexer


-- | Similar to @reserved@ exept that it asserts that the symbol is not
-- followed by characters conformant to regular operators.
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- | @parens p@ creates a parser that succeeds only if @p@ is parenthesised.
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Parser that consumes whitespace
whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

-- | The full parser, capable of parsing an entire expression.
-- The function buildExpressionParser performs the job of building
-- this parser with respect to the structural features and their
-- precedence order, defined in @table@.
expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"

-- | Parse many expressions seperated by ";"
expressions :: Parser [Expr]
expressions = endBy expr (Tok.lexeme lexer $ char ';')

-- | Table of specific parsers for structural langage features. As these
-- features behave much like operators in other languages, they are
-- encoded as operators.
table :: [[Operator String () Identity Expr]]
table = [ [Infix  (do e <- variable
                      return $ \e' e'' -> Application e'' (Application e' e)) AssocLeft]
        , [Infix  (reservedOp "|"   >> return Application) AssocLeft]
        , [Infix  (reservedOp "->"  >> return Lambda) AssocRight]
        , [Infix  (reservedOp ":="  >> return Assign) AssocLeft]
        ]

-- | Parser for identifiers/variables.
variable :: Parser Expr
variable = Var <$> identifier

-- | Simple language terms, which are either entire parenthesised
-- expressions or of the simples type, identifiers/variables.
term :: Parser Expr
term =  parens expr
    <|> variable
    -- <|> Var <$> operator
    <?> "term"

-- | Try parsing the supplied string, returing either
-- the parser failed, @Left@, or the parsed expression, @Right@.
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whiteSpace >> expr <* eof) "<stdin>"

-- | Try parsing multiple expressions from the supplied string, returing either
-- the parser failed, @Left@, or the parsed expressions, @Right@.
parseExpressions :: String -> Either ParseError [Expr]
parseExpressions = parse (whiteSpace >> expressions <* eof) "<stdin>"

-- | Parse a source-file containing expressions seperated by ";".
parseFile :: String -> IO (Either ParseError [Expr])
parseFile file = do
  contents <- readFile file
  return $ parse (whiteSpace >> expressions <* eof) file contents
