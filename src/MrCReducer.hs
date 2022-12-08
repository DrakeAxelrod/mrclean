module MrCReducer where

import           MrCParser (Expr (..))
import           Text.Read (readMaybe)

traverseLeafs :: (Expr -> Expr) -> Expr -> Expr
traverseLeafs f (Application l r) = Application (traverseLeafs f l) (traverseLeafs f r)
traverseLeafs f (Lambda a b) = Lambda a (traverseLeafs f b)
traverseLeafs f (Assign x e) = Assign x (traverseLeafs f e)
traverseLeafs f e = f e

convertInt :: Expr -> Expr
convertInt = traverseLeafs f
    where f (Var n) = maybe (Var n) Number (readMaybe n)
          f e       = e
