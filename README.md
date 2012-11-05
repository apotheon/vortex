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
### Lexer
The lexer is pretty much complete when it comes to language's current design.
More keywords will be added and operators might change.
### Parser
- Parse chunks of code. __[yes]__
- Parse blocks. __[yes]__
- Parse basic expressions. __[yes]__
- Parse suffix expressions. [partial]
- Parse operator precedences. __[yes]__
- Parse functions. __[yes]__
- Parse coroutines and generators. [no]
- Parse let expression. __[yes]__
- Parse patterns. __[yes]__
- Parse match expression. __[yes]__
- Parse conditionals. __[yes]__
- Parse while loops. __[yes]__
- Parse do-while loops. __[yes]__
- Parse numeric for loops. __[yes]__
- Parse generic for loops. __[yes]__
- Parse quotes. __[yes]__
- Parse tables. __[yes]__
- Parse sequences. __[yes]__
- Parse lists. [no]
- Parse additional table operations. [no]
- Parse objects. [no]
- Parse macros. [no]
### Macro processor
No work on macros has been done yet.
### Codegen
### Runtime
### Standard library

More descriptive table coming later.

## Example code

See code examples in the documentation/wiki.
