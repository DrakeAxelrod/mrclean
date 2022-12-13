module MrCReducer where

import           Data.HashMap (Map, empty, delete, insert)
import qualified Data.HashMap as HashMap
import qualified MrCParser
import           Text.Read    (readMaybe)
import           Control.Monad.State (State)
import           Control.Applicative (liftA2, (<|>))

data Expr = Var String
          | Number Double
          | Application Expr Expr
          | Lambda String Expr
          | Assign String Expr
          | Implicit ImplicitFun
          deriving (Show)

newtype ImplicitFun = ImplicitFun (Expr -> Either String Expr)

instance Show ImplicitFun where
    show _ = "ImplicitFunction"

data ImplicitFunS = ImplicitFunS ImplicitFun String -- 6

implicit_op :: (Double -> Double -> Double) -> String -> ImplicitFunS
implicit_op op n = ImplicitFunS (ImplicitFun f) n
    where f  (Number x)             = Right $ Implicit $ ImplicitFun $ f' x
          f  _                      = Left e
          f' x (Number y)  = Right $ Number (x `op` y)
          f' _ _                    = Left e
          e                         = "invalid argument type for implicit function '" ++ n ++ "'"

implicit_operators :: [ImplicitFunS]
implicit_operators = [ implicit_op (+) "_implicit_add"
                     , implicit_op (-) "_implicit_sub"
                     , implicit_op (*) "_implicit_mul"
                     , implicit_op (/) "_implicit_div"
                     ]

implicit_map :: Map String ImplicitFun
implicit_map = foldl (\ m (ImplicitFunS f s) -> insert s f m) empty implicit_operators


-- | Convert the parser expression-tree into the reduction expression-tree
convertExpr :: MrCParser.Expr -> Either String Expr
convertExpr (MrCParser.Var s) = Right $ maybe v Number (readMaybe s)
    where v = maybe (Var s) Implicit $ HashMap.lookup s implicit_map
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
type MachineState = Either (Either String Machine) Machine -- Result<Machine, String>

type Result = Either String Expr

initMachine :: Expr -> Machine
initMachine e = Machine empty e []

-- T, (e p), S ==> T, e, p : S
-- T, y -> e, p : S ==> T e[p/y], S
-- T[p |-> e], p, S ==> T, e, #p : S
-- T, y -> e, #p : S ==> T[p |-> y -> e], y -> e, S

-- 3 | ((expr) | (+))
-- (expr) | (x -> y -> x+y) [3]
-- (x -> y -> x+y) [(expr), 3]
-- (y -> (expr)+y) [3]
-- (expr)+3 :=

ruleSet :: Machine -> MachineState
ruleSet (Machine heap control stack) = 
    case (control, stack) of
        (Application p e, _) -> Right $ Machine heap e (Right p : stack)
        (Lambda y e, Right p : s) -> Right $ Machine heap (substitute y p e) s
        (Implicit f, Right p : s) -> ruleSetImplicit f p s heap
        (Var p, _) -> let d   = (Left $ Left ("unknown variable " ++ p)) 
                          f e = Machine (delete p heap) e (Left p : stack) 
                      in maybe d (Right . f) $ HashMap.lookup p heap
        (s, []) -> Left $ Right $ Machine heap control stack
        (s, a) -> Left $ Left $ "No Rules Apply " ++ show s ++ " " ++ show a


ruleSetImplicit :: ImplicitFun -> Expr -> VarStack -> VarHeap -> MachineState
ruleSetImplicit (ImplicitFun f) p s h = ms' ms
    where ms = reduceFull $ Machine h p []
          ms' (Left (Right (Machine h' p' _))) = either (Left . Left) (\e -> Right $ Machine h' e s) (f p')
          ms' x = x

reduceFull :: Machine -> MachineState
reduceFull m = reduceFull' $ ruleSet m
    where reduceFull' (Right m') = reduceFull m'
          reduceFull' ms         = ms


substitute :: String -> Expr -> Expr -> Expr
substitute s e' (Var x) | x == s = e'
                        | otherwise = Var x
substitute s e' (Lambda x e) | x == s    = Lambda x e
                             | otherwise = Lambda x (substitute s e' e)
substitute s e' (Application e1 e2) = Application (substitute s e' e1) (substitute s e' e2)
substitute s e' (Assign x e) = Assign x (substitute s e' e)
substitute _ _ e = e

-- | Reduce the machine state until it is in a final state
reduce' :: MachineState -> Either String Expr
reduce' (Right m) = reduce' (ruleSet m)
reduce' (Left (Left s)) = Left s
reduce' (Left (Right (Machine _ e _))) = Right e

reduce :: Machine -> Either String Expr
reduce = reduce' . Right
