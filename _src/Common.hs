module Common (
    Expr (..), 
    InterpError, 
    Interp, 
    FuncImplicit (..),
    MRCValue (..)
    ) 
where

type InterpError = String
type Interp = Either InterpError Expr

newtype FuncImplicit = FuncImplicit (Expr -> Interp)

instance Show FuncImplicit where
    show f = "~FuncImplicit~"

data MRCValue = MRCNum Double
    deriving (Eq, Show)

data Expr = ExprValue MRCValue 
          | ExprVar String
          | ExprApply Expr Expr
          | ExprAssign String Expr
          | ExprFun String Expr
          | ExprImplicit FuncImplicit
          deriving (Show)

