Implementation
==============

Here I will outline what my plans are for the implementation of the Keen compiler.


- Specify the abstract syntax, omitting macros, type classes, families and kinds
- Implement a parser, to make it possible to experiment with the syntax
- Implement a code emitter, to experiment with the semantics (except the type indexed stuff)
- Implement a type checker
- Extend the above to support macros, type classes, families and kinds
