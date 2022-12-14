-- DEPRECATED
import MrCParser

import Test.QuickCheck

-- | 
statements :: [(String, Expr, Bool)]
statements = [
  ("x := y", Assign (Var "x") (Var "y"), True),
  ("x -> x", Lambda (Var "x") (Var "x"), True),
  ("x|y", Application (Var "x") (Var "y"), True),
  ("v", Var "v", True),
  ("x := (x -> x)", Assign (Var "x") (Lambda (Var "x") (Var "x")), True),
  ("y := (f -> (f|f))", Assign (Var "y") (Lambda (Var "f") (Application (Var "f") (Var "f"))), True),
  ("z := (f -> ((f|f)|f))", Assign (Var "z") (Lambda (Var "f") (Application (Application (Var "f") (Var "f")) (Var "f"))), True),
  ("1 + 2", Application (Var "2") (Application (Var "1") (Var "+")), True),
  ("1 + 2 * 3 = 7", Application (Application (Application (Application (Application (Application (Var "1") (Var "+")) (Var "2")) (Var "*")) (Var "3")) (Var "=")) (Var "7"), True),
  ("(1 + 2) * 3 = 9", Application (Application (Application (Application (Application (Application (Var "1") (Var "+")) (Var "2")) (Var "*")) (Var "3")) (Var "=")) (Var "9"), True),
  ("1 + 2 * 3", Application (Application (Application (Application (Var "1") (Var "+")) (Var "2")) (Var "*")) (Var "3"), True),
  ("1 + (2 * 3)", Application (Application (Var "1") (Var "+")) (Application (Application (Var "2") (Var "*")) (Var "3")), True),
  ("(1 + 2) * 3", Application (Application (Application (Application (Var "1") (Var "+")) (Var "2")) (Var "*")) (Var "3"), True),
  ("x := 1 + 2", Assign (Var "x") (Application (Application (Var "1") (Var "+")) (Var "2")), True),
  ("x := 1 + 2 | 3", Assign (Var "x") (Application (Application (Application (Var "1") (Var "+")) (Var "2")) (Var "3")), True),
  ("x := 1 | (y -> y + 1) | 2", Assign (Var "x") (Application (Application (Var "1") (Lambda (Var "y") (Application (Application (Var "y") (Var "+")) (Var "1")))) (Var "2")), True)
  ]

testParseExpr :: String -> Expr -> Bool
testParseExpr s e = case parseExpr s of
  Left _ -> False
  Right e' -> e == e'


testAllStatements :: [(String, Expr, Bool)] -> Bool
testAllStatements [] = True
testAllStatements ((s, e, b):xs) = testParseExpr s e == b && testAllStatements xs

prop_allStatements :: Bool
prop_allStatements = testAllStatements statements

main :: IO ()
main = do
  -- test allTestStatements
  quickCheck prop_allStatements
  -- show the one that fails
  print $ filter (\(s, e, b) -> not $ testParseExpr s e) statements
