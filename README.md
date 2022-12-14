# mrclean

Haskell runtime for clean derivative language

<!-- <img src="./assets/giphy.gif" width="100%" height="100%" /> -->

## Proposal Text
A Pure Functional language implementation relying on graph-reduction of syntax-trees 
to reduce/evaluate expressions. The project will include
* defining a simple language syntax, 
* creating a parser with the library parsec to parse source code into syntax-tree(s),
* implementing the reduction algorithm to compute expressions
* and creating a simple gui (using a suitable library) that allows the user to 
    input text, have that expression be evaluated and the answer displayed.

Minimal goals (of the language) consist of being able to
* compute basic algebraic expressions (eg `1 + 2`),
* save results to variables (eg `x := 1 + 2`)
* and create/use functions (`f := x -> x + 2` and then `1 | f`)

The GUI should fill it's purpose. However, not much effort will be 
made in order to make it look fancy.

Further features might include varying amounts of *syntactic-sugar* and
language features (such as: more datatypes, data structures, symbolic computation etc),
depending on how much time it takes.

## Authors

- [Drake Axelrod](https://draxel.io)
- [Hugo Lom](https://bit.ly/2xwTlrb)


## References
- [Parsec](https://hackage.haskell.org/package/parsec)
- [Clean](https://en.wikipedia.org/wiki/Clean_(programming_language))
- [Graph-Reduction (concept)](https://en.wikipedia.org/wiki/Graph_reduction)
