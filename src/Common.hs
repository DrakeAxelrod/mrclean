module Common (Expr (..)) where

data Expr = ExprVar String 
          | ExprApp Expr Expr 
          deriving (Show)
