# LiterateFStar

Export F* code as LaTeX.

Patches F* to add the primitive `comments_of_module`, which adds the
ability to lookup the comments associated with a module.

Then a F* meta-program withing F* extract code blocks and comments so
that it is rendered as LaTeX.

## How to use
This repo uses a specific F* build, with a few patches (see
`/patches`).

`nix run .#build` will build that F* version and build the example
`Literate.Example`.

Otherwise, `nix shell` will get you a shell with the correct, patched
F*.

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

