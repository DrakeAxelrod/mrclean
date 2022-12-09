module MrCReducer where

import           Data.HashMap (HashMap, empty)
import           MrCParser    (Expr (..))
import           Text.Read    (readMaybe)

-- | The heap holding named references to expressions
type VarHeap = HashMap String Expr

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

-- s = Left "this error happened"


traverseLeafs :: (Expr -> Expr) -> Expr -> Expr
traverseLeafs f (Application l r) = Application (traverseLeafs f l) (traverseLeafs f r)
traverseLeafs f (Lambda a b) = Lambda a (traverseLeafs f b)
traverseLeafs f (Assign x e) = Assign x (traverseLeafs f e)
traverseLeafs f e = f e

convertInt :: Expr -> Expr
convertInt = traverseLeafs f
    where f (Var n) = maybe (Var n) Number (readMaybe n)
          f e       = e

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
