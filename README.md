# Scheme Bytecode Interpreter
This repo includes the source code implementing bytecode to interpret subset of scheme. It was developed to learn how bytecode is designed and implemented for a programming language. The repo contains the shell script `run.sh` to run the test. The script is only tested using Chez scheme.

The development initially starts by implementing a simple interpreter and then iteratively convert the interpreter until it can interpret by mean of bytecode. The development flow depicts as below figure.

![interpreter-relationship.svg](/interpreter-relationship.svg)


The testing also use the implemented interpreter load the interpreter source code and run the test cases. It tests whether the interpreter can interpret itself.

Besides the interpreter, it also implement convienient features: pattern matching and quasiquote.

# Reference
- Lisp in Small Pieces by Christian Queinnec
- https://srfi.schemers.org/srfi-200/srfi-200.html (pattern matching)
- Quasiquotation in Lisp by Alan Bawden
- Revised Report on the Algorithmic Language
Scheme (R5RS,R6RS,R7RS)
