# structure-editor

Prototype an `∃(L : Langs). IDE L` that has good discoverability.

## Language

```
Types
t ::= Nat | String | t → t | ([l : t](,)) | ([l : t](|))

Expressions
e ::= x | fun p begin e end | e e
    | let p = e; e
    | letrec p = e; e
    | ([l = e](,))
    | match e [when l = p then e] end
    | (e : t)

Pattern
p ::= x | ([b](,))
Binder
b ::= x | x as p
```

Bi-directional type checking

Unit, records, variants, functions.

Take accessibility into account.

## Logs 2021-01-26

Auto-completion can improve discoverability considerably. I'd like to employ this mechanism to "teach" users syntaxes.

I need a middle layer between AST and HTML. The middle layer may be translated to whitespace-indented string or a single line description if necessary.

A parser is absolutely necessary.