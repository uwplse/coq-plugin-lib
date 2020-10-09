This is a library of useful utility functions for Coq plugins.
These functions originally come from
from the accompanying project archives (TODO: NAMES),
but may be useful for plugin development more broadly.

To build this library with a test plugin, run:

```
./build.sh
```

See accompanying project (TODO: NAMES) archive for examples of loading it as a submodule.

## Guide

* [build.sh](/build.sh): Build script for example plugin
* [theories](/theories): Example plugin that loads the library
* [src](/src): Main source directory
  - [plib.mlpack](/src/plib.mlpack)
  - [plibrary.ml4](/src/plibrary.ml4): Top-level file
  - [utilities](/src/utilities): General OCaml utilities
  - [coq](/src/coq): Coq utilities
    - [constants](/src/coq/constants): Useful Coq constants, like sigma types and equality
    - [devutils](/src/coq/devutils): Utilities for development, like printing terms with deBruijn indices
    - [logicutils](/src/coq/logicutils): Utilities for the core logic of Coq
      - [contexts](/src/coq/logicutils/contexts): Contexts, environments, and state for unification
      - [hofs](/src/coq/logicutils/hofs): Useful higher-order functions, like flexible reduction and substistution functions
      - [inductive](/src/coq/logicutils/inductive): Inductive types
      - [transformation](/src/coq/logicutils/transformation): Transforming types and terms
      - [typesandequality](/src/coq/logicutils/typesandequality): Type checking & inference, unification, convertibility, and so on
    - [representationutils](/src/coq/representationutils): Definitions, names, and dealing with different representations of terms
    - [termutils](/src/coq/termutils): Utilities for constructing and reasoning about certain kinds of terms, like constants and functions
