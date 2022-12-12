module MrCParser (Expr (..), parseExpr) where

import           Data.Functor.Identity (Identity)
import           Text.Parsec           (ParseError, eof, many, noneOf, oneOf,
                                        parse, (<?>), (<|>))
import           Data.Char             (isAlphaNum, isAscii)
import           Data.Either           (isRight)
import           Text.Parsec.Expr      (Assoc (AssocLeft),
                                        Operator (Infix, Prefix),
                                        buildExpressionParser)
import           Text.Parsec.Language  (emptyDef)
import           Text.Parsec.Prim      ()
import           Text.Parsec.String    (Parser)
import qualified Text.Parsec.Token     as Tok

import           Test.QuickCheck       (Gen, Arbitrary, arbitrary, sample, listOf1, oneof, sized)
import           Control.Monad         (liftM, liftM2)

data Expr = Var String
          | Application Expr Expr
          | Lambda Expr Expr
          | Assign Expr Expr
          deriving (Show, Eq)

isValidIdent :: Char -> Bool
isValidIdent c = isAlphaNum c && isAscii c

validChar :: Gen Char
validChar = validChar' arbitrary
  where validChar' gc = do c <- gc
                           if isValidIdent c then 
                            return c
                           else 
                            validChar' arbitrary

validIdent :: Gen String
validIdent = listOf1 validChar

genExpr :: Gen Expr
genExpr = sized genExpr'

instance Arbitrary Expr where
  arbitrary = genExpr

genExpr' :: Int -> Gen Expr
genExpr' 0 = liftM Var validIdent
genExpr' n | n > 0 = oneof [ liftM Var validIdent
                           , liftM2 Application (liftM Var validIdent) subexpr
                           , liftM2 Lambda (liftM Var validIdent) subexpr
                           , liftM2 Assign (liftM Var validIdent) subexpr
                           ]
  where subexpr = genExpr' (n `div` 2)


prop_parseExprValid :: Expr -> Bool
prop_parseExprValid = isRight . parseExpr . printExpr

printExpr :: Expr -> String
printExpr (Var s) = s
printExpr (Application lhs rhs) = "( " ++ printExpr lhs ++ " | " ++ printExpr rhs ++ " )"
printExpr (Lambda arg body) = "( " ++ printExpr arg ++ " -> " ++ printExpr body ++ " )"
printExpr (Assign var body) = "( " ++ printExpr var ++ " := " ++ printExpr body ++ " )"

{- | MrClean Language Definition
  * Defines all of the language features using the Text.Parsec.Token module.
  * We use the emptyDef as a base, and overwrite the features we want to change.
-}
mrCleanDef :: Tok.LanguageDef st
mrCleanDef = emptyDef
          {
            Tok.commentStart    = "/*"                    -- ^ start of a block comment.
          , Tok.commentEnd      = "*/"                    -- ^ end of a block comment.
          , Tok.commentLine     = "//"                    -- ^ line comment.
          , Tok.nestedComments  = True                    -- ^ the language supports nested block comments.
          , Tok.identStart      = noneOf "\n \t\r\f|()"   -- ^ don't allow any of these characters to start an identifier.
          , Tok.identLetter     = noneOf "\n \t\r\f|()"   -- ^ don't allow any of these characters to be part of an identifier.
          , Tok.opStart         = oneOf ""                -- ^ not used in this language.
          -- | This parser should accept any legal tail characters of operators.
          -- Note that this parser should even be defined if the language doesn't
          -- support user-defined operators, or otherwise the 'reservedOp'
          -- parser won't work correctly.
          , Tok.opLetter        = oneOf ""                -- ^ not used in this language.
          , Tok.reservedOpNames = ["|" , "->", ":=", "$"] -- ^ reserved operators.
          , Tok.reservedNames   = ["|" , "->", ":=", "$"] -- ^ reserved names.
          , Tok.caseSensitive   = False                   -- ^ case insensitive.
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

-- | @reserved name@ Parser for detecting symbol @name@, asserting that
-- the symbol is not directly followed by characters conformant to general
-- identifiers, thus behaving like a reserved keyword.
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

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

expressions :: Parser [Expr]
expressions = many expr
-- | Table of specific parsers for structural langage features. As these
-- features behave much like operators in other languages, they are
-- encoded as operators.
table :: [[Operator String () Identity Expr]]
table = [ [Prefix (reservedOp "$"   >> return (Application (Var "reduce")))]
        -- TODO: Actually make this prefix
        -- , [Prefix (do e <- variable
        --               return $ \e' -> Application e' e)]
        , [Infix  (do e <- variable
                      return $ \e' e'' -> Application (Application e' e) e'') AssocLeft]
        , [Infix  (reservedOp "|"   >> return Application) AssocLeft]
        , [Infix  (reservedOp "->"  >> return Lambda) AssocLeft]
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

-- | Try parsing the supplied string, returing wether
-- the parser failed, @Left@, or the parsed expression, @Right@.
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whiteSpace >> expr <* eof) "<stdin>"

parseExpressions :: String -> Either ParseError [Expr]
parseExpressions = parse (whiteSpace >> expressions <* eof) "<stdin>"

-- | Parse a source-file containing a single expression.
parseFile :: String -> IO (Either ParseError Expr)
parseFile file = do
  contents <- readFile file
  return $ parse (whiteSpace >> expr <* eof) "" contents


-- | Load an parse a test file "assets/test.mrc"
testFile :: IO (Either ParseError Expr)
testFile = parseFile "assets/test.mrc"

failingParse = Application (Var "FL3q[!v]K<") (Application (Var "$+\\9A&M*~") (Lambda (Var "*z~") (Lambda (Var "/*>?") (Var "m`3%#{W"))))
