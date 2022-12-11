module Interpreter () where

import Text.Read (readMaybe)
import Data.Fixed (mod')
import Control.Applicative (liftA2)
import Common (Expr (..), MRCValue (..), FuncImplicit (..), Interp, InterpError)
import Data.Maybe (isNothing)
import Data.Bifunctor (bimap)


convertValue' :: Expr -> Expr
convertValue' (ExprVar v) = maybe (ExprVar v) (ExprValue . MRCNum) (readMaybe v)
convertValue' e = e

convertValue :: Expr -> Expr
convertValue (ExprValue v) = ExprValue v
convertValue (ExprVar v) = convertValue' (ExprVar v)
convertValue (ExprApply lhs rhs) = ExprApply (convertValue lhs) (convertValue rhs)
convertValue (ExprAssign n e) = ExprAssign n (convertValue e)
convertValue (ExprFun n b) = ExprFun n (convertValue b)

implicitBinNum' :: (Double -> Double -> Double) -> String -> (Expr -> Expr -> Interp)
implicitBinNum' op n = f
    where f (ExprValue (MRCNum x)) (ExprValue (MRCNum y)) = Right $ ExprValue (MRCNum (x `op` y))
          f lhs rhs = Left ("invalid types for \'" ++  n ++ "\'")

createImplicitBinary :: (Expr -> Expr -> Interp) -> Expr
createImplicitBinary f = ExprImplicit $ FuncImplicit g
    where g e = pure $ ExprImplicit $ FuncImplicit (\e' -> f e e')



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
convertImplicit (ExprApply lhs rhs) = ExprApply (convertImplicit lhs) (convertImplicit rhs)
convertImplicit (ExprAssign n e) = ExprAssign n (convertImplicit e)
convertImplicit (ExprFun n b) = ExprFun n (convertImplicit b)
convertImplicit e = e

-- | `traceChange c f g` where `f` and `g` are functions that
-- indicate via `Maybe` if the input was changed, will modify `c`
-- so that it returns `Nothing` if neither of the functions
-- changed their input
traceChange ::
    (a -> b -> c) -> 
    (a -> Maybe a) ->
    (b -> Maybe b) -> 
    a -> b -> Maybe c
traceChange c f g x y | isNothing x' && isNothing y' = Nothing
                      | otherwise                    = Just (c x'' y'')
    where x'  = f x
          y'  = g y
          x'' = maybe x id x'
          y'' = maybe y id y'

-- expr -> maybe interp
reduceExpr' :: (Expr -> Maybe Interp) -> Interp -> Maybe Interp
reduceExpr' f = g
    where g (Left _)  = Nothing
          g (Right e) = f e

reduceExpr :: Expr -> Maybe Interp
reduceExpr (ExprApply lhs (ExprImplicit (FuncImplicit f))) = Just (f lhs)
reduceExpr (ExprApply lhs rhs) = 
    traceChange (liftA2 ExprApply) (reduceExpr' reduceExpr) (reduceExpr' reduceExpr) (pure lhs) (pure rhs)
reduceExpr (ExprAssign n e) = 
    traceChange (liftA2 ExprAssign) (const Nothing) (reduceExpr' reduceExpr) (pure n) (pure e)
reduceExpr (ExprFun n b) = 
    traceChange (liftA2 ExprFun) (const Nothing) (reduceExpr' reduceExpr) (pure n) (pure b)
reduceExpr e = Nothing


exhaust :: (a -> Maybe a) -> a -> a
exhaust f a = case (f a) of
                Just a' -> exhaust f a'
                Nothing -> a