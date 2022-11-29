module Common (Expr (..)) where

data Expr = ExprVar String 
          | ExprApp Expr Expr
          | ExprAssign String Expr
          | ExprFun String Expr
          deriving (Show)
