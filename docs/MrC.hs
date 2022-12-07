-- Path: MrC.hs
module MrC where

import MrCParser
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
import Data.Functor.Identity (Identity)

data Expr = Var String
          | Application Expr Expr
          | Lambda Expr Expr
          | Assign Expr Expr
          deriving (Show)

data Value = VVar String
           | VApplication Value Value
           | VLambda Value Value
           | VAssign Value Value
           | VInt Int
           | VBool Bool
           deriving (Show)

data Type = TVar String

data TypeEnv = TypeEnv [(String, Type)]

data Env = Env [(String, Value)]

typeOf :: TypeEnv -> Expr -> Type
typeOf (TypeEnv env) (Var x) = case lookup x env of
  Just t -> t
  Nothing -> error $ "Unbound variable: " ++ x

typeOf env (Application e1 e2) = case typeOf env e1 of
  TVar "->" -> case typeOf env e2 of
    TVar x -> TVar x
    _ -> error "Type error in application"
  _ -> error "Type error in application"

typeOf env (Lambda e1 e2) = case typeOf env e1 of
  TVar x -> case typeOf env e2 of
    TVar y -> TVar "->"
    _ -> error "Type error in lambda"
  _ -> error "Type error in lambda"

typeOf env (Assign e1 e2) = case typeOf env e1 of
  TVar x -> case typeOf env e2 of
    TVar y -> TVar y
    _ -> error "Type error in assignment"
  _ -> error "Type error in assignment"

eval :: Env -> Expr -> Value
eval (Env env) (Var x) = case lookup x env of
  Just v -> v
  Nothing -> error $ "Unbound variable: " ++ x
eval env (Application e1 e2) = case eval env e1 of
  VLambda e3 e4 -> eval env (subst e3 e4 e2)
  _ -> error "Type error in application"
eval env (Lambda e1 e2) = VLambda e1 e2
eval env (Assign e1 e2) = case eval env e1 of
  VVar x -> case eval env e2 of
    v -> Env ((x, v):env)
    _ -> error "Type error in assignment"
  _ -> error "Type error in assignment"

subst :: Expr -> Expr -> Expr -> Expr
subst (Var x) e2 (Var y) = if x == y then e2 else Var y
subst e1 e2 (Application e3 e4) = Application (subst e1 e2 e3) (subst e1 e2 e4)
subst e1 e2 (Lambda e3 e4) = Lambda (subst e1 e2 e3) (subst e1 e2 e4)
subst e1 e2 (Assign e3 e4) = Assign (subst e1 e2 e3) (subst e1 e2 e4)

