# Micro-ML
Parser combinator in [fparsec](https://www.quanttec.com/fparsec/) for Micro-ML,
a tiny programming language used in [Programming Language
Concepts](http://www.itu.dk/people/sestoft/plc/) by Peter Sestoft.

The parser combinator is mostly equivalent to the parser generator fslex/fsyacc
found i Sestoft's
[fun1.zip](http://www.itu.dk/people/sestoft/plc/firstedition/fun1.zip) but with
the HigherFun evaluator from [fun2.zip](http://www.itu.dk/people/sestoft/plc/firstedition/fun2.zip).

The interpreter can be run on a file from the examples directory with `make run
program=examples/<file>`, the AST will be printed and evaluated

Small test programs can be found in `/tests/test.fs` and can be run with `make
test`
