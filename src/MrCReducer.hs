{- Lab 4
   Date: 2022-12-14
   Authors: Hugo Lom, Drake Axelrod
   Lab group: 68
 -}
module MrCReducer where

import           Data.HashMap (Map, empty, delete, insert)
import qualified Data.HashMap as HashMap
import qualified MrCParser
import           Text.Read    (readMaybe)
import           Control.Applicative (liftA2)

-- | The expressions tree is represented
data Expr = Var String
          | Number Double
          | Application Expr Expr
          | Lambda String Expr
          | Assign String Expr
          | Implicit ImplicitFun
          deriving (Show)

-- | The implicit function data type, allowed to fail
-- via the @Either@ data type. The only interface to any
-- Haskell functions.
newtype ImplicitFun = ImplicitFun (Expr -> Either String Expr)

-- | An explicit implementation of the @Show@ class with a constant
-- representation such that the @Expr@ tree can be displayed.
instance Show ImplicitFun where
    show _ = "ImplicitFunction"

-- | A helper data structure for storing the implicit function implementation
data ImplicitFunS = ImplicitFunS ImplicitFun String

-- | Helper function for creating named binary, numeric, implicit functions.
implicitOp :: (Double -> Double -> Double) -> String -> ImplicitFunS
implicitOp op n = ImplicitFunS (ImplicitFun f) n
    where f  (Number x)             = Right $ Implicit $ ImplicitFun $ f' x
          f  _                      = Left e
          f' x (Number y)           = Right $ Number (x `op` y)
          f' _ _                    = Left e
          e                         = "Runtime Error: Invalid argument '" ++ n ++ "'"

-- | The implicit operators definitions
implicitOperators :: [ImplicitFunS]
implicitOperators = [ implicitOp (+) "_implicit_add"
                    , implicitOp (-) "_implicit_sub"
                    , implicitOp (*) "_implicit_mul"
                    , implicitOp (/) "_implicit_div"
                    ]

-- | Mapping of implicit operators to their names
implicitMap :: Map String ImplicitFun
implicitMap = foldl (\ m (ImplicitFunS f s) -> insert s f m) empty implicitOperators


-- | Convert the parser expression-tree into the reduction expression-tree
convertExpr :: MrCParser.Expr -> Either String Expr
convertExpr (MrCParser.Var s) = Right $ maybe v Number (readMaybe s)
    where v = maybe (Var s) Implicit $ HashMap.lookup s implicitMap
convertExpr (MrCParser.Application lhs rhs) = 
    liftA2 Application (convertExpr lhs) (convertExpr rhs)
convertExpr (MrCParser.Lambda (MrCParser.Var n) rhs) = 
    Lambda n <$> convertExpr rhs
convertExpr (MrCParser.Lambda e _) = 
    Left ( "Conversion Error: Invalid variable expression in lambda `" ++ 
           show e ++ 
           "`"
         )
convertExpr (MrCParser.Assign (MrCParser.Var n) rhs) = 
    Assign n <$> convertExpr rhs
convertExpr (MrCParser.Assign e _) = 
    Left ( "Conversion Error: Invalid variable expression in assignment `" ++ 
            show e ++ 
            "`"
         )

-- | Convert the reducer expression-tree back into the parser expression-tree
convertExprBack :: Expr -> MrCParser.Expr
convertExprBack (Var s) = MrCParser.Var s
convertExprBack (Number n) = MrCParser.Var $ show n
convertExprBack (Application lhs rhs) =
    MrCParser.Application (convertExprBack lhs) (convertExprBack rhs)
convertExprBack (Lambda n rhs) = 
    MrCParser.Lambda (MrCParser.Var n) (convertExprBack rhs)
convertExprBack (Assign n rhs) = 
    MrCParser.Assign (MrCParser.Var n) (convertExprBack rhs)
convertExprBack (Implicit _) = MrCParser.Var "ImplicitFun"

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
data Machine = Machine VarHeap Expr [Either String Expr] 
    deriving (Show)

-- | The machine state represents either an active @Machine@
-- or either a successfull reduction or the error message.
type MachineState = Either (Either String Machine) Machine

-- | Rule Set for reducing the machine state, as described in 
-- @Deriving a lazy abstract machine@.
ruleSet :: Machine -> MachineState
ruleSet (Machine heap control stack) = 
    case (control, stack) of
        (Application p e, _) -> Right $ Machine heap e (Right p : stack)
        (Lambda y e, Right p : s) -> Right $ Machine heap (substitute y p e) s
        (Implicit f, Right p : s) -> ruleSetImplicit f p s heap
        (Var p, _) -> let d   = (Left $ Left ("Runtime Error: Unknown variable `" ++ p ++ "`")) 
                          f e = Machine (delete p heap) e (Left p : stack) 
                      in maybe d (Right . f) $ HashMap.lookup p heap
        (Assign v e, []) -> Left $ Right $ Machine (insert v e heap) (Var v) []
        (e, Left v : s) -> Right $ Machine (insert v e heap) e s
        (_, []) -> Left $ Right $ Machine heap control stack
        (s, a) -> Left $ Left $ "Runtime Error: No Rules Apply!\n\tHeap: " ++ show heap ++ "\n\tControl: " ++ show s ++ "\n\t Stack: " ++ show a

-- | The implicit function rule set
ruleSetImplicit :: ImplicitFun -> Expr -> VarStack -> VarHeap -> MachineState
ruleSetImplicit (ImplicitFun f) p s h = ms' ms
    where ms = reduceFull $ Machine h p []
          ms' (Left (Right (Machine h' p' _))) = either (Left . Left) (\e -> Right $ Machine h' e s) (f p')
          ms' x = x

-- | Substitute the expression @e'@ for the variable @s@ in the expression @e@
substitute :: String -> Expr -> Expr -> Expr
substitute s e' (Var x) | x == s = e'
                        | otherwise = Var x
substitute s e' (Lambda x e) | x == s    = Lambda x e
                             | otherwise = Lambda x (substitute s e' e)
substitute s e' (Application e1 e2) = Application (substitute s e' e1) (substitute s e' e2)
substitute s e' (Assign x e) = Assign x (substitute s e' e)
substitute _ _ e = e

-- | Reduce the machine state until it is in a final state
reduceFull :: Machine -> MachineState
reduceFull m = reduceFull' $ ruleSet m
    where reduceFull' (Right m') = reduceFull m'
          reduceFull' ms         = ms

-- | Helper function for @runMany@, manages the list of remaining expressions,
-- the result of the most recent evaluation and the heap. 
runMany' :: VarHeap -> [Expr] -> (MachineState, VarHeap, [Expr])
runMany' h [e] = (reduceFull $ Machine h e [], empty, [])
runMany' h (e : r) = f $ reduceFull $ Machine h e []
    where f (Left (Right (Machine h' _ _))) = runMany' h' r
          f m = (m, empty, [])
runMany' _ [] = (Left $ Left "Runtime Error: No expression to reduce", empty, [])

-- | A statefull reduction of many expressions. The heap is maintained throughout
-- the many expression evaluations.
runMany :: [Expr] -> MachineState
runMany es = f $ runMany' empty es
    where f (m, _, _) = m
