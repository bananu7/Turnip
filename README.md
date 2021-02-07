# This is Turnip.

[![Circle CI](https://circleci.com/gh/bananu7/Turnip.svg?style=svg)](https://circleci.com/gh/bananu7/Turnip)

Turnip is an attempt at fully pure Lua VM writen in Haskell. It started as a compiler project (which got stuck around the AST phase) by a different person.

Since then it grew over the years as a vehicle for learning language development. The parser is currently able to parse almost the entirety of Lua (modulo some minor corner cases), and the evaluator supports base language primitives as well as a lot of builtin/standard functions.

The main design goals for Turnip are ease of implementation and correctness. Performance is specicifically not addressed yet at all; this means that some operations will be hilariously inefficient.

That being said, it could already be useful for you if:
 * you need just a well-tested Lua source Parser
 * you want to run simple scripts within a pure environment
 * you want to try extending or modifying Lua and need an easily editable codebase
