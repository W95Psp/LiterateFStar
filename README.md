# LiterateFStar

Export F* code as LaTeX.

Patches F* to add the primitive `comments_of_module`, which adds the
ability to lookup the comments associated with a module.

Then a F* meta-program withing F* extract code blocks and comments so
that it is rendered as LaTeX.

## Load other module / raw LaTeX
Include the LaTeX generated for a given module:
```
//!open Some.FStar.Module
```

Include a raw file:
```
//!raw path
```

Render declarations with a custom (F*) filter:
```
//@printer=Literate.Example.e_capitalizer
let somedef = ...
```

Hide a definition
```
//@hide
let hiddendef = ...
```

Hide qualifiers
```
//@hide-quals
unfold let hiddendef = ...
```

## More example
See `Literate.Example.fst`, `Literate.Example.A.fst` and `Literate.Example.B.fst`.

