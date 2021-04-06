module Literate.Example

module A = Literate.Example.A
module B = Literate.Example.B

open FStar.Tactics
open Literate

// example of a custom renderer that tweak a definition using a JS script
let mk_js_renderer code: renderer
  = fun _ _ s -> launch_process "node" ["-e";"let s = require('fs').readFileSync('/dev/stdin').toString(); " ^ code ^ "; process.stdout.write(s)"] (s ())

let e_capitalizer: renderer = mk_js_renderer "
   s = s.replace(/e/g, 'E');
"

let _ = modul_to_latexFile "example.tex" ["Literate";"Example";"A"]

