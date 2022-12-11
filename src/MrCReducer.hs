module MrCReducer where

import           Data.HashMap (Map, empty, delete)
import qualified Data.HashMap as HashMap
import qualified MrCParser
import           Text.Read    (readMaybe)
import           Control.Monad.State (State)
import           Control.Applicative (liftA2, (<|>))

data Expr = Var String
          | Number Int
          | Application Expr Expr
          | Lambda String Expr
          | Assign String Expr
          deriving (Show, Eq)


-- | Convert the parser expression-tree into the reduction expression-tree
convertExpr :: MrCParser.Expr -> Either String Expr
convertExpr (MrCParser.Var s) = Right $ maybe (Var s) Number (readMaybe s)
convertExpr (MrCParser.Application lhs rhs) = (liftA2 Application) (convertExpr lhs) (convertExpr rhs)
convertExpr (MrCParser.Lambda (MrCParser.Var n) rhs) = Lambda n <$> convertExpr rhs
convertExpr (MrCParser.Lambda e _) = Left ("Invalid variable expression in lambda `" ++ show e ++ "`")
convertExpr (MrCParser.Assign (MrCParser.Var n) rhs) = Assign n <$> convertExpr rhs
convertExpr (MrCParser.Assign e _) = Left ("Invalid variable expression in assignment `" ++ show e ++ "`")


-- | The state of the machine is the heap, the control expression, and the stack
-- Rules (T is the heap, S is the stack. i : S means that the top item in the stack is i, T[p |-> e] means that there exists a binding between variable p to expression e in the heap)
-- #p is an indication that the control value should be bound to p in the heap
-- e[p/y] means substitute y with p in expression e
-- state ==> new state
-- T, (e p), S ==> T, e, p : S
-- T, y -> e, p : S ==> T e[p/y], S
-- T[p |-> e], p, S ==> T, e, #p : S
-- T, y -> e, #p : S ==> T[p |-> y -> e], y -> e, S
-- im pretty sure control is the switch
-- newtype State s a = State { runState :: s -> (a, s) } if this is true, then we should use the (VarHeap, Expr, VarStack) ie State (VarHeap, Expr, VarStack) (Expr)
-- mrcState :: State (VarHeap, Expr, VarStack) Expr -- i think the State s a is 
-- mrcState = do
--     (heap, control, stack) <- get
--     case control of
--         return control -- to be implemented


-- | The heap holding named references to expressions
type VarHeap = Map String Expr

-- | A stack item is either an indication to add 
-- the control expression to the heap or an expression
-- as the argument to the control
type StackItem = Either String Expr

-- | The stack is a list of @StackItem@s
type VarStack = [StackItem]

-- | The machine is the @VarHeap@, the control-expression and
-- the execution stack
data Machine = Machine VarHeap Expr [Either String Expr] deriving (Show)

-- | The machine state holds the @Machine@ variables or
-- error info
type MachineState = Either String Machine -- Result<Machine, String>

type Result = Either String Expr

cloneExpr :: MachineState -> Either String Expr 
cloneExpr (Left s) = Left s
cloneExpr (Right (Machine _ c _)) = Right c

initMachine :: Expr -> MachineState
initMachine e = Right $ Machine empty e []

-- T, (e p), S ==> T, e, p : S
-- T, y -> e, p : S ==> T e[p/y], S
-- T[p |-> e], p, S ==> T, e, #p : S
-- T, y -> e, #p : S ==> T[p |-> y -> e], y -> e, S

-- app1 :: Machine -> MachineState
-- app1 (Machine h (Application p e) s) = return $ Machine h e (Right p : s)
-- app1 (Machine _ c _) = Left "app1 not applicable to " ++ show c
-- 
-- app2 :: Machine -> MachineState
-- app1 (Machine h (Lambda y e) (Right p : s)) = return $ Machine h (substitute y p e) s
-- app1 (Machine _ c _) = Left "app1 not applicable to " ++ show c

ruleSet :: MachineState -> MachineState
ruleSet (Left s) = Left s
ruleSet (Right (Machine heap control stack)) = 
    case (control, stack) of
        (Application p e, _) -> Right $ Machine heap e (Right p : stack)
        (Lambda y e, Right p : s) -> Right $ Machine heap (substitute y p e) s
        (Var p, _) -> let d   = (Left ("unknown variable " ++ p)) 
                          f e = Machine (delete p heap) e (Left p : stack) 
                      in maybe d (Right . f) $ HashMap.lookup p heap
        _ -> Left "Not Implemented"

traverseLeafs :: (Expr -> Expr) -> Expr -> Expr
traverseLeafs f (Application l r) = Application (traverseLeafs f l) (traverseLeafs f r)
traverseLeafs f (Lambda a b) = Lambda a (traverseLeafs f b)
traverseLeafs f (Assign x e) = Assign x (traverseLeafs f e)
traverseLeafs f e = f e

substitute :: String -> Expr -> Expr -> Expr
substitute s e' (Var x) | x == s = e'
                        | otherwise = Var x
substitute s e' (Lambda x e) | x == s    = Lambda x e
                             | otherwise = Lambda x (substitute s e' e)
substitute s e' (Application e1 e2) = Application (substitute s e' e1) (substitute s e' e2)
substitute s e' (Assign x e) = Assign x (substitute s e' e)
substitute _ _ e = e

validApplication :: Expr -> Bool
validApplication (Application (Lambda _ _) _) = True
validApplication _                            = False

-- | The example function  (1 | (x -> x | y))
example :: Expr
example = Application (Number 1) (Lambda "x" (Application (Var "x") (Var "y")))