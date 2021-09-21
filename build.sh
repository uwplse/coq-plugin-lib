opam pin dune 2.7.1
opam pin coq-serapi 8.9.0+0.6.1
opam install lymp
dune clean
dune build @all
dune build @all
dune install
