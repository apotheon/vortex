# Vortex

Vortex is a high level scripting language that compiles to Lua.

The current compiler is written in Lua and is far from complete; it's still
very much work in progress. The codegen will get separated, more features will
be added, code updated, reformatted, cleaned up and documented. The next
implementation of Vortex will be self hosted.

## Features

Vortex is a dynamic (untyped) language (and strongly checked). It puts
emphasis on procedural and functional programming. It also features prototype
based OO with possible implementation of class based through extension of
syntax and semantics.

It primarily draws inspiration from Lua, Scheme, OCaml/F# and Rust.

It can be integrated into any project that already uses Lua very easily
(through provided API). A custom standard library is to be written and
Vortex scripts will live in their own environment not polluting the Lua
global namespace. Interaction between Lua and Vortex is possible using
a special table.

The language uses a curly braced, free form syntax with optional semicolons.
Everything is an expression; there are no pure statements (althrough some
of the expressions are meant to be used in statement forms).

A quick list of features:

- Most of Lua's features including table magic
- Lua-like semantics
- Convenient functions with easy default argument values
- Implicit and explicit returns
- Blocks are expressions
- Coroutines and generators
- Lightweight objects
- AST macros
- Extensions of syntax and semantics
- Pattern matching
- Sequences
- And more

The performance is comparable to handwritten Lua.

## Status

It can compile and run a few examples. Mostly incomplete. A few approximations:

|Component       |Status|
|----------------|------|
|Formal grammar  |90%   |
|Lexer           |90%   |
|Parser          |60%   |
|Codegen         |30%   |
|Runtime         |20%   |
|Standard library|0%    |

More descriptive table coming later.

## Example code

See code examples in the documentation/wiki.
