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

Thanks to its Lua base, you can easily embed Vortex in your project. Every
Vortex script lives within its own environment, no outer modifications are
made. Vortex uses Lua's standard library. New modules are to be written
though, so the standard library will be extended.

Any Lua module can be accessed through a special table. Vortex's module
system will use custom routines for this, handling loading of macros and
so on, which will be incompatible with Lua's. If you want to use Lua
modules, use Lua functions.

The language uses a curly braced, free form syntax. That allows very easy
implementation, but creates a few constraints. Everything is an expression;
there are no statements (althrough some of the expressions are meant to be
used as statements).

A quick list of features:

- Most of Lua's features including table magic
- Lua-like semantics
- Convenient functions with easy default argument values
- Blocks are expressions
- Coroutines and generators
- Lightweight, multiple-inheritance, delegative, prototypal object system
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
- Parse suffix expressions. __[yes]__
- Parse operator precedences. __[yes]__
- Parse functions. __[yes]__
- Parse coroutines and generators. [no]
- Parse let expression. __[yes]__
- Parse with expression. __[yes]__
- Parse patterns. __[yes]__
- Parse match expression. __[yes]__
- Parse conditionals. __[yes]__
- Parse while loops. __[yes]__
- Parse do-while loops. __[yes]__
- Parse numeric for loops. __[yes]__
- Parse generic for loops. __[yes]__
- Parse quotes and unquotes. __[yes]__
- Parse tables. __[yes]__
- Parse sequences. __[yes]__
- Parse lists. __[yes]__
- Parse postfix conditionals. __[yes]__
- Parse enumerations. __[yes]__
- Parse additional table operations. __[yes]__
- Parse objects. __[yes]__
- Parse interpolated strings and nested expressions. __[yes]__
- Parse macros. [no]

### Macro processor

No work on macros has been done yet.

### Codegen

### Runtime

### Standard library

Vortex uses Lua's standard library. It will be extended with custom modules
and some will be redesigned, but the core modules (string, math, ...) stay
(sometimes with extensions).

## Example code

See code examples in the documentation/wiki.
