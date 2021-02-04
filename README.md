# structure-editor

Prototype an `∃(L : Langs). IDE L` that has good discoverability.

## Why structural editing?

Two motivations:

* Code completion can really increase the discoverability of a PL. If we consider code completion dropboxes as lightweight search boxes and increase their capacity, we almost got a strucutred editor!
* Structured editors are really good at making errors local. This is great because local errors are way easier to remove. Examples of localizing bugs include confining unparsable code snippets in AST holes.

Random thoughts on existing structured editors:

* This one is for envision. Envision replaces many keywords with symbols. I don't think this is a good idea because they should be some ways to read programs. Using symbols make source code harder to READ OUT LOUD, which, I personally think, is super helpful in understanding the code.

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

## Credits

TeXmacs, for lots of inspiration.

I really enjoy the following two artifacts, especially [2], which gives a fair accounts of history and lessons.

[1]: [Envision: reinventing the integrated development environment](https://www.research-collection.ethz.ch/handle/20.500.11850/214522)
[2]: [Evolution of novice programming environments: The structure editors of Carnegie Mellon University](https://www.researchgate.net/profile/John_Pane/publication/243575656_Evolution_of_Novice_Programming_Environments_The_Structure_Editors_of_Carnegie_Mellon_University/links/00b4952cfe48157c6a000000.pdf)