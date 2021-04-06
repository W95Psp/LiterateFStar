/// \documentclass{article}
/// \usepackage[outputdir=latex.out]{minted}
/// \begin{document}

/// \section{Literate.Example.A}
module Literate.Example.A

open FStar.Tactics

/// Some text, for \texttt{Literate.Example.A} module.


/// Text for $foo_a0$, line 1

/// $foo_a0$ line 2
/// $foo_a0$ line 3
//@printer=Literate.Example.e_capitalizer
//@hide-quals
unfold let foo_a0 = 
  ();
  (); // inline double slash comment
  (); 98 + 123

irreducible let ggg = 1

/// Now, we want to show \texttt{Literate.Example.B}:
//!open Literate.Example.B
open Literate.Example.B

/// Text for $foo_a1$, line 1

/// $foo_a1$ line 2
/// $foo_a1$ line 3
unfold let foo1 = 123
private let foo2 = 234324

/// \end{document}

