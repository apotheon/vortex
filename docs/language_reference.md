# The Vortex programming language
## Language reference, version 0.1
### Introduction
Vortex is a high level scripting language supporting multiple programming
paradigms, suited for general use, embedding purposes and data description.
It shares many of its goals with Lua and its default implementation compiles
to Lua code (LuaJIT recommended for optimal performance). While the current
implementation is written in Lua, the next one will be self-hosted, written
in Vortex itself.

Vortex provides good support for a combination of procedural, object oriented
(prototype based with class based implementable in a library) and functional
(impure) paradigms. It draws inspiration from Lua itself, Scheme/Lisp,
OCaml/F# and Rust. Like Lua and Scheme and unlike OCaml/F# and Rust,
it's an untyped, but strongly checked language.

The compiler, interpreter and runtime are free and open source software,
provided under the terms of the MIT license.
### Disclaimer
The language is very much a work in progress; this document reflects the
current state. I don't guarantee a stable language specification. Backwards
compatibility is not retained.
### Conventions
The formal grammar descriptions in this document are defined in an extended
dialect of BNF. All example code and grammar blocks are specified in
`monospace` blocks. The example code follows the defined Vortex style.
The grammar definitions may contain regular expressions enclosed in
`/regexp/`. Sometimes, character ranges may be used, such as
`'a-Z'` or `'0-9'`. Those typically represent something like
`'a' | 'b' | 'c' .....`.
### Tokenization
Vortex uses a free form syntax. The source is treated as a token stream
without any emphasis on indentation or whitespace. All whitespace is
ignored in the resulting token stream, serving only as a token
delimiter where feasible.
#### Encoding
    alpha     ::= 'A-Za-z'
    digit     ::= '0-9'
    hex_digit ::= '0-9a-fA-F'
The source text is read byte-by-byte. The language is specified with ASCII;
things like string literals are encoding dependent. It's recommended to use
UTF-8.
#### Whitespace
    whitespace ::= ' ' | '\n' | '\r' | '\t' | '\f' | '\v'
Either a regular space, a newline, a carriage return, a tab or a form feed
characters are treated whitespace. Carriage return and newline can be used
in pairs `\r\n` and `\n\r` respectively or alone to separate lines.
#### Comments
    comment ::= '//' /.*$/ | '/*'  { /.*?/ | comment } '*/'
Vortex uses regular C/C++ style comments. Long comments can span multiple
lines. Short comments comment everything until the end of the line. The
comment text is ignored by the Vortex lexer, but it assumes long comment
nesting.
#### Identifiers
    ident ::= ('_' | alpha) { '_' | alpha | digit }
Only ASCII characters are allowed in identifiers that are alphanumeric
plus underscore. Digits can't start an identifier.
#### Keywords
    keyword ::= '__FILE__'
              | '__LINE__'
              | 'again'
              | 'and'
              | 'as'
              | 'break'
              | 'case'
              | 'cfn'
              | 'coro'
              | 'do'
              | 'else'
              | 'false'
              | 'fn'
              | 'for'
              | 'glob'
              | 'goto'
              | 'if'
              | 'in'
              | 'let'
              | 'match'
              | 'mod'
              | 'nil'
              | 'not'
              | 'or'
              | 'quote'
              | 'rec'
              | 'return'
              | 'seq'
              | 'true'
              | 'when'
              | 'while'
              | 'yield'
Keywords are used in the language and can't be used as identifiers. New
keywords may be defined using macros and syntax extensions.
#### Number literals
    number_literal ::= ('0x' | '0X') { hex_digit } [ '.' ] { hex_digit } [ ('p' | 'P') [ '+' | '-' ] digit ]
                     | { digit } [ '.' ] { digit } [ ('e' | 'E') [ '+' | '-' ] digit ]
    expr ::= number_literal
Vortex does not separate integral and floating point types, instead there is
just one type called "number". Numbers have an integral and floating point
forms. When a number literal is prefixed with `0x` or `0X` it's a hexadecimal
number. Floating point literals smaller than one can omit the 0 at the start
(so that `.6` is a valid form of `0.6`; hex constants behave similarly, i.e.
`0x.5` is a valid form). Optional decimal exponent is marked with `e` or `E`
(binary exponent in hex constants is `p` or `P`).
#### String literals
    string_short   ::= ? any line ? { ('\\z' | '\\') ? any line ? }
    string_literal ::= '"' string_short '"' | '\'' string_short '\''
                     | '[' { n * '=' } '[' ? any string except the closing brackets ? ']' { n * '=' } ']'
    string_escape  ::= '\\a' | '\\b' | '\\f' | '\\n' | '\\r' | '\\t' | '\\v' | '\\z' | '\\' | '\\"' | '\\\''
    expr ::= string_literal
String literals follow Lua rules. Short string literals evaluate escape
sequences specified in `string_escape`. The quote and apostrophe escape
sequences only need to be escaped in certain cases (in `"foo"`, you need
to escape `\"`, in `'foo'` you need to escape `\'`). You may break a short
string literal into multiple lines using `\\` (the real newline after the
backslash will be included in the string). `\\z` works similarly, but it
does not include the newline and also skips all whitespace until the first
non-whitespace character after that (useful for removing indentation).

A byte following a backslash (in regular decimal form from 0 to 255 or `xYY`
in hexadecimal form where YY is the number) represents a code point embedded
in the string. Embedded zeros are permitted.

Long string literals are written using Lua rules. A regular long string has a
format `'[[string contents]]`. It can't be nested, for next level you use a
format `[=[another level]=]`. You proceed similarly with deeper levels; you
just add another separator. Long string literals can contain anything except
their closing brackets. Escape sequences in them are not interpreted during
tokenization. Any form of newline (carriage return, newline or a combination
of both in any order) in them is converted to a simple newline. If a long
string starts with a newline (right after the opening bracket) it's ignored
for convenience.
#### Booleans and nil
    boolean_literal ::= 'true' | 'false'
    expr ::= boolean_literal | 'nil'
Simple boolean true/false constants. Expressions in conditionals etc. evaluate
to boolean values. Nil value is "nothing". Undefined (but declared) variables
have a nil value. The result of logical not on a nil is "true". Nil and false
are the only two values that evaluate to false.
#### Other tokens
    op_binary_nkw ::= '+' | '-' | '*' | '/' | '%' | '**' | '='
                    | '..' | '==' | '<' | '<=' | '!=' | '>' | '>='
                    | '+=' | '-=' | '*=' | '/=' | '%=' | '^=' | '++' | '::' | '++=' | '**='
                    | '&' | '|' | '^' | '<<' | '>>' | '&=' | '|=' | '^=' | '<<=' | '>>='
    op_unary_nkw  ::= '-' | '#' | '~'
    tok_other     ::= '(' | ')' | '{' | '}' | '[' | ']' | ':' | '.' | ',' | ';' | '...' | '->' | '|' | '$'
### Expressions
    special_expr ::= '$' '(' expr ')'
    expr ::= special_expr
Vortex is a fully expression based language; there are no statements.
Statement-like expressions either jump in the code flow (like `break`,
`again` or `goto`) or evaluate to `nil`. Expressions in Vortex should
be familiar to people working with languages Vortex is inspired by
(especially Lua people should feel comfortable). "Special expressions"
written as `$(expr)` are used in certain places to denote that we really
want to evaluate an expression in the parens and that it's not a typo (for
example table key expressions, expression patterns or inside strings for
interpolation). In other places special expressions can be used
interchangeably with parenthesised expressions (for example,
`5 + 10 * $(1 + 2)` is the same as `5 + 10 * (1 + 2)`)
#### Blocks and chunks
    ident_list ::= ident { ',' ident }
    expr_list  ::= expr  { ',' expr  }
    chunk      ::= { expr }
    block      ::= '{' { expr } [ '->' (expr | ( '(' expr_list ')' )) ] '}'
    expr       ::= ';' | block
A chunk is a sequence of expressions. A Vortex source file is a chunk. Chunks
behave like anonymous functions. They can have local variables, they can have
environments, they can return values. This is used by Vortex's module system.

A block is a sequence of expressions enclosed in curly braces. A block itself
is an expression. It's executed when it's evaluated, functions can make use of
this. When used with functions, you can return a value from a block at some
specific position using the `return` keyword. If the block is ended with a
`->` followed by either one expression or a list of expressions in parenthesis,
the block evaluates to that expression (or expressions) in expression form, in
statement form it equals explicit `return`.

A semicolon is an expression evaluating to nil. It can be used to separate
statement-form expressions in blocks and chunks when required.

#### Variables
    variable      ::= ident | prefix_expr '[' expr ']' | prefix_expr '.' ident
    variable_list ::= variable { ',' variable } | prefix_expr '[' expr_list ']'
                    | prefix_expr '.' '(' ident_list ')'
Vortex has, like Lua, three types of variables - global variables, local
variables and table fields. An identifier can be used to access a global
or a local variable. Access to an undeclared variable is considered an
error. Variables follow lexical scoping rules.

You can use square brackets to index a table. Multiple comma-separated
expressions in the square brackets result in multiple values, thus
`foo["a"], foo["b"], foo["c"]` is equivalent to `foo["a", "b", "c"]`.
If the member you're accessing has a name in form of an identifier,
you can use a dot (so `foo.bar` is equivalent to `foo["bar"]`). You
can use parens for multiple member access (`foo.a, foo.b, foo.c` is
equivalent to `foo.(a, b, c)`).

Metatables can be used to control table indexing. Global variables are
members of a global table called `_G`, so that accessing `foo` is
equivalent to `_G["foo"]`. This can be used to perform reflection.
#### Let expression
    let_expr ::= 'let' [ 'rec' | 'glob' ] ( pattern | ( '(' pattern_list ')' ) )
                 [ '=' ( expr | ( '(' expr_list ')' ) ]
You use the `let` expression to declare (and define) variables. Vortex unlike
Lua uses the "strict" mode, any variable must be declared before you can
assign it. A `let` expression is formed by using the `let` keyword along
with optional `rec` (recursive binding where the value is aware of itself)
or `glob` (makes a global variable instead of local variable, a global
variable is always aware of itself) keywords, followed by a list of
patterns (see pattern matching, one or more) and optional assignment part
(consisting of the assignment operator and a list of expressions with always
at least one expression). If you don't provide the assignment part, the variables
will be nil. Thus doing `let foo = nil` and `let foo` is equivalent.

To avoid syntax ambiguities, you have to put the sides of the `let` expression
into parentheses if they contain more than one pattern/expression.

You can shadow `let` expressions. They always evaluate to the value of the
variable (or a list of variables).

You can use a limited set of patterns (only non-conditional patterns, that is,
in this case variable pattern, table pattern and cons pattern). Table pattern
always matches in this case, no matter if the elements exist. You may not use
any other patterns except variable patterns in the `let` expression.
#### If expression
    expr_branch ::= '->' expr | '(' expr_list ')' | block
    if_expr ::= 'if' expr expr_branch [ 'else' (expr | expr_branch) ]
An `if` expression is a conditional expression. It evaluates a condition and
if it can be converted to a boolean `true` value, it evaluates to the "true"
expression(s). If it can't, it evaluates to either nil (if no `else` branch is
specified) or the "false" expression(s).

If you need multiple conditions, you can simply put another `if` expression in
the `else` branch.

When working with blocks, writing `->` constantly can feel superfluous and
decrease readability. You can omit it in such cases.

You can also avoid the arrow after `else` even if there is no block. That makes
it convenient to do `else if`. However, when working with expressions in the
`else` you'll typically want it (as a separator).
#### Loop expressions
    loop_expr ::= 'while' expr expr_branch | 'do' expr_branch 'while' expr
                | 'for' ident_list 'in' expr expr_branch
                | 'for' ident '=' expr, expr [ ',' expr ] expr_branch
    expr ::= 'break' | 'again' | loop_expr
There are three types of loops in Vortex.

A `while` loop evaluates as long as its condition converts to the `true` value.
A `do-while` loop is similar, except that it always iterates at least once
before checking the condition.

You can use the `break` and `again` keywords in loops. The `break` keyword
stops the loop. The `again` keyword skips to the next iteration (very much
like `continue` in C).

A `for` loop has two forms like in Lua with identical semantics. The numeric
form looks like `for var=start, end[, step] -> expr`. It begins iteration at
start and ends at end. It increments by step each iteration, step defaults to 1.
The generic form looks like `for ident_list in iter -> expr`. Again, it performs
identically to Lua and is compatible with all existing Lua iterators.
#### Functions
    fn_arg  ::= ident [ '=' expr ]
    fn_args ::= fn_arg { ',' fn_arg }
    fn_expr ::= 'fn' fn_args (expr_branch | ('->' match_body))
    expr ::= fn_expr
Functions take one or more arguments and evaluate to one or more results. There
are no "named" functions, only variables with function values (functions are
first class objects). They are transparent closures with access to all their
outer scopes. Functions do not yield, for this purpose there are generators
(specialized form) and fibers (cooperative threads). Functions have a special
pattern matching form, described later in the part about pattern matching.
#### Sequences
    seq_expr ::= 'seq' expr_branch
    expr ::= 'yield' ( expr | ( '(' [ expr_list ] ')' ) ) | seq_expr
Sequences evaluate to zero or more values using yielding. Given an expression:

    let a, b, c = seq { for i in range(1, 3) -> yield i }

`a` will be `1`, `b` will be `2` and `c` will be `3`.
Sequences are coroutines implementation-wise. They are resumed as long as they
are not dead; any yielded value is appended to the results.
#### Quotes
    quote_expr ::= 'quote' expr_branch
    expr ::= quote_expr
Quotes in Vortex are regular Lisp-like quotes. You can quote any expression.
Basically, instead of evaluating it, you get the AST representation of the
given expression which you can further process. You can later evaluate it,
but note that any references to local variables outside the quote will be lost.
#### Match expression
    pattern ::= string_literal | number_literal | boolean_literal | ident | pattern 'as' ident
              | pattern 'and' pattern | pattern 'or' pattern | 'not' pattern | ident '::' ident
              | '[' { (pattern | (ident '=' pattern) | (special_expr '=' pattern)) } ']'
              | special_expr | '_' | 'nil' | pattern 'when' expr
    pattern_list ::= pattern { ',' pattern }
    match_body ::= { '|' pattern_list expr_branch }
    match_expr ::= 'match' expr_list (('->' match_body) | ('{' match_body '}'))
Vortex implements pattern matching not too dissimilar to its equivalent in
languages like OCaml. The syntax is similar as well and the range of supported
patterns is wide.

Patterns are "rules" used to match some kind of input data. Using patterns
(like in the `match` expression but also elsewhere) you can create simpler
and more elegant code than when using traditional conditionals.

The `match` expression generalizes over the well known `switch` statement.
It consists of the `match` keyword, a list of input expressions followed by
`->` (or `{`, but don't forget to close the match appropriately) and a `match`
expression body. The body consists of a list of pattern-expression pairs called
"arms" or "branches".

Each arm begins with the `|` token, followed by a list of patterns (usually
the same amount of patterns as the amount of input expressions, but you may
omit some sometimes). The pattern list is followed by a normal expression
branch. Patterns are evaluated from first to last and the first arm that
matches the given input(s) is evaluated. The whole `match` expression then
evaluates to that arm (any other is skipped).

Unlike a `switch` statement, there is no fallthrough for the `match`
expression. Only one arm at time is evaluated. However, most uses for
fallthrough are eliminated because of and/or patterns.

Patterns can have 'guards' which are basically conditional expressions. For example,
`| var when var < 10` maches input as var, but only when it's smaller than 10.

You don't use anything to separate the arms, the `|` already serves as a clean
separator. You may use the `case` keyword in its place if you prefer.

Here is the list of supported patterns:
- **Expression pattern:**
Any string, number, boolean, nil (`"foo"`, `'hello'`, `10`, `true`, `false`, `nil`)
or special expression form (`$(expr)`)
- **Variable pattern:**
An identifier: `foobar`
- **As pattern:**
`[ a, b ] as c`
- **AND pattern:**
`pattern1 and pattern2`
- **OR pattern:**
`pattern1 or pattern2`
- **Cons pattern:**
`first :: rest`
- **Table pattern:**
`[ pattern1, pattern2, key = pattern, $(expr) = pattern ]`
- **Wildcard pattern:**
Matches anything: `_`

#### Coroutine functions
    cfn_expr ::= 'cfn' fn_args (expr_branch | ('->' match_body))
    expr ::= cfn_expr
Coroutine functions are similar to regular functions. Syntactically, the only
difference is a keyword change, `cfn` instead of `fn`. This change gives
functions the ability to yield values in a manner similar to coroutines.
When you yield, the coroutine function is "paused" and arguments passed to
yield are returned from the call. A next call of the coroutine function
resumes it at the yield point, making yield return any values passed to
the call. When the function ends (or you return explicitly) it returns
the last expression (or expression list), just like regular functions.
Note that coroutine functions have (same as coroutines) limited lifetime
and when they end, they're dead and cannot be restored. You can avoid this
behavior by having an infinite loop in the coroutine function and yielding
when required.

They type of a coroutine function is a regular function.

An example:

    let fun = cfn a, b {
        // first call, a and b are 5 and 10
        // prints 100 and 200, the first call returns 6 and 11
        let a, b = yield(a + 1, b + 1)
        print(a, b) // a and b are 100 and 200, from the second call
        print yield(a + 2, b + 2) // makes it return 102 and 202
        // dead now, can't call
    }
    print fun(5, 10) // prints 6 and 11
    print fun(100, 200) // prints 102 and 202

Note that coroutine functions and coroutines are slower than regular functions.
## Coroutines
    coro_expr ::= 'coro' fn_args (expr_branch | ('->' match_body))
    expr ::= coro_expr
Similar to coroutine functions, but generalized. Their type is "thread". You
can't call them. You can "resume" them (which calls the coroutine from the last
yield point). Resume behaves similarly to a coroutine function call but it
returns a boolean value as the first result followed by all others (true if
the coroutine can be resumed again, false if it's dead now).
