# MrClean

made right & clean

```haskell

data Expr = Number Int | Variable String | Add Expr Expr | Mult Expr Expr | CallFunction String [Expr] | Function [String] Expr derives (Show)
foo(x, y+1) --> CallFunction "foo" [Variable "x", Add (Variable "y") (Number 1)] -->
foo(x, y) = x + y --> Function ["x", "y"] (Add (Variable "x") (Variable "y"))
reduceVal :: Exrp -> Int
reduceVal (Add (Number n) (Number m)) = reduce Number (n + m)
reduceVal (Number n) = n

tree = Add (Number 1) (Number 2) -- === 1 + 2
btree = tree `Add` (Number 3) -- === (1+2)+3

a = Add (Number n) (Number m)
b = a $ Add (Number n) (Number m)

expr = Add (Number n) (Number M)

reduce :: nargs -> result

z <- (expr)

f: f(x) + 1 == x -> f(x) = x - 1

10 + 10 -> 20

a = 10
foo(x, y)
~(x, y) -> foo(x + y, x)

substitute :: Expr -> String -> Expr -> Expr
substitute (Variable varname) varname_replace replace_tree | varname == varname_replace = replace_tree
substitute (Add lhs rhs) varname_replace replace_tree = Add (replace lhs varname_replace replace_tree) (replace rhs varname_replace replace_tree)

foo(x) = x

foo(2) = 2


```

## Syntax

$ is evaluation / reduction / call / invoke
| is pipe
() is scope
:= is assignment
-> is function declaration
\d+ is integer
// is single line comment

/\* \*/ is multiline

## References

- [Parsec](https://hackage.haskell.org/package/parsec)
- [Clean](<https://en.wikipedia.org/wiki/Clean_(programming_language)>)
- [Graph-Reduction (concept)](https://en.wikipedia.org/wiki/Graph_reduction)
