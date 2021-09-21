This is a library of useful utility functions for Coq plugins. These functions originally come from [PUMPKIN PATCH](https://github.com/uwplse/PUMPKIN-PATCH) and [DEVOID](https://github.com/uwplse/ornamental-search), but may be useful for plugin development more broadly.

To build this library with a test plugin, run:

```
./build.sh
```

See [PUMPKIN PATCH](https://github.com/uwplse/PUMPKIN-PATCH) and [DEVOID](https://github.com/uwplse/ornamental-search) for examples of loading it as a submodule.

## Guide

* [LICENSE](/LICENSE): License
* [README.md](/README.md): You are here!
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
      
## Contributors

This library was developed by Talia Ringer, Nate Yazdani, and RanDair Porter. Probably none of it would build without Emilio's help.

## Licensing

We use the MIT license because we think Coq plugins have a right not to use GPL.
If this is wrong, please let us know kindly so we can fix this.
