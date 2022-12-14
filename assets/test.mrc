/*
  this is a block comment
*/

// this is a single line comment

// this assigns the lambda(y) where we apply y to 1
// x := y -> y + 1;

// define simple math operators

+ := _implicit_add;
* := _implicit_mul;
- := _implicit_sub;
/ := _implicit_div;

// foo := x -> x + 2;

quadratic := a -> b -> c -> x -> (a * x * x) + (b * x) + c;


main := 10 | (x | (10 | (2 | quadratic)));

// main := 1 | x;
// answer 303
main;
