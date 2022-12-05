module Common (Expr (..)) where

import Text.Read (readMaybe)
import Text.Show (Show)
import Data.Fixed (mod')

newtype FuncImplicit = FuncImplicit (Expr -> Expr)

instance Show FuncImplicit where
    show f = "~FuncImplicit~"

data MRCValue = MRCNum Double
    deriving (Eq, Show)

data Expr = ExprValue MRCValue 
          | ExprVar String
          | ExprApp Expr Expr
          | ExprAssign String Expr
          | ExprFun String Expr
          | ExprImplicit FuncImplicit
          deriving (Show)

convertValue :: Expr -> Expr
convertValue (ExprVar v) = maybe (ExprVar v) (ExprValue . MRCNum) (readMaybe v)
convertValue e = e

convertExprToValue :: Expr -> Expr
convertExprToValue (ExprValue v) = ExprValue v
convertExprToValue (ExprVar v) = convertValue (ExprVar v)
convertExprToValue (ExprApp lhs rhs) = ExprApp (convertExprToValue lhs) (convertExprToValue rhs)
convertExprToValue (ExprAssign n e) = ExprAssign n (convertExprToValue e)
convertExprToValue (ExprFun n b) = ExprFun n (convertExprToValue b)

implicitBinNum' :: (Double -> Double -> Double) -> String -> (Expr -> Expr -> Expr)
implicitBinNum' op n = f
    where f (ExprValue (MRCNum x)) (ExprValue (MRCNum y)) = ExprValue (MRCNum (x `op` y))
          f lhs rhs = error ("invalid types for \'" ++  n ++ "\'")

createImplicitBinary :: (Expr -> Expr -> Expr) -> Expr
createImplicitBinary f = ExprImplicit $ FuncImplicit g
    where g e = ExprImplicit $ FuncImplicit (\e' -> f e e')

implicitBinNum f s = createImplicitBinary $ implicitBinNum' f s

valueToBool' :: Double -> Bool
valueToBool' n = n /= 0.0

boolToValue' :: Bool -> Double
boolToValue' True  = 1.0
boolToValue' False = 0.0

valueToBool :: MRCValue -> Bool
valueToBool (MRCNum n) = valueToBool' n

boolToValue :: Bool -> MRCValue
boolToValue = MRCNum . boolToValue'


(<.>) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
g <.> f = \x y -> g $ f x y

implicitAdd = implicitBinNum (+) "add"
implicitSub = implicitBinNum (-) "sub"
implicitDiv = implicitBinNum (/) "div"
implicitMul = implicitBinNum (*) "mul"
implicitMod = implicitBinNum mod' "mod"
implicitPow = implicitBinNum (**) "pow"
implicitEq  = implicitBinNum (boolToValue' <.> (==)) "eq"
implicitNeq = implicitBinNum (boolToValue' <.> (/=)) "neq"
implicitGt  = implicitBinNum (boolToValue' <.> (>)) "gt"
implicitLt  = implicitBinNum (boolToValue' <.> (<)) "lt"
implicitGte = implicitBinNum (boolToValue' <.> (>=)) "gte"
implicitLte = implicitBinNum (boolToValue' <.> (<=)) "lte"

-- implicitfloor = implicitBinNum floor "floor"
-- Boolean logic?
{-
-}

convertImplicit' :: Expr -> Expr
convertImplicit' (ExprVar "_implicit_add") = implicitAdd
convertImplicit' (ExprVar "_implicit_sub") = implicitSub
convertImplicit' (ExprVar "_implicit_div") = implicitDiv
convertImplicit' (ExprVar "_implicit_mul") = implicitMul
convertImplicit' (ExprVar "_implicit_mod") = implicitMod
convertImplicit' (ExprVar "_implicit_pow") = implicitPow
convertImplicit' (ExprVar "_implicit_eq")  = implicitEq
convertImplicit' (ExprVar "_implicit_neq") = implicitNeq
convertImplicit' (ExprVar "_implicit_gt")  = implicitGt
convertImplicit' (ExprVar "_implicit_lt")  = implicitLt
convertImplicit' (ExprVar "_implicit_gte") = implicitGte
convertImplicit' (ExprVar "_implicit_lte") = implicitLte
convertImplicit' e = e

convertImplicit :: Expr -> Expr
convertImplicit (ExprVar v) = convertImplicit' $ ExprVar v
convertImplicit (ExprApp lhs rhs) = ExprApp (convertImplicit lhs) (convertImplicit rhs)
convertImplicit (ExprAssign n e) = ExprAssign n (convertImplicit e)
convertImplicit (ExprFun n b) = ExprFun n (convertImplicit b)
convertImplicit e = e

reduceExpr :: Expr -> Expr
reduceExpr (ExprApp lhs (ExprImplicit (FuncImplicit f))) = f lhs
reduceExpr (ExprApp lhs rhs) = ExprApp (reduceExpr lhs) (reduceExpr rhs)
reduceExpr (ExprAssign n e) = ExprAssign n (reduceExpr e)
reduceExpr (ExprFun n b) = ExprFun n (reduceExpr b)
reduceExpr e = e
