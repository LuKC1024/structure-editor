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