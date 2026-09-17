# Lambda AST Encoding and Evaluation

As evaluation of lambda expressions is itself a computational task,
it stands to reason that this can also be expressed as a lambda term.
This project encodes the abstract syntax tree of lambda expressions as other lambda terms,
using the so-called "Morgensen-Scott encoding".
An evaluator expression is defined such that, when applied to an encoding of a lambda term $M$,
yields a term that is semantically equivalent to $M$.

Terms use **de Bruijn indices**, so variables are represented by integers:

```ocaml
type term = Var of int | Lam of term | App of term * term
```

For example, the identity function `λx. x` is represented as:

```ocaml
Lam (Var 0)
```
## Project layout

```text
lib/
├── term.ml                 Lambda-term type and de Bruijn shifting
├── encode.ml               Scott/Mogensen-Scott AST encoding
├── eval.ml                 Beta reduction and encoded evaluator
├── utils.ml                Shared term pretty-printing
└── data/
    ├── nat.ml              Church natural-number encoding
    └── lambda_list.ml      Church list operations

bin/main.ml                 Executable examples
test/test_lambda_eval.ml    Round-trip tests
```

## Building and running

```sh
dune build
dune exec bin/main.exe
```

The example executable prints the original term, its encoded representation,
and the result after evaluating the representation.

## Testing

```sh
dune test
```

The test compares the normal form of a given term against that of the
evaluation of the representation of that form.

## Using the library

```ocaml
open Lambda_eval.Term
open Lambda_eval.Encode
open Lambda_eval.Eval
open Lambda_eval.Utils

let identity = Lam (Var 0)
let encoded = encode identity
let evaluated = normalize 1000 (App (e_eval, encoded))

let () = print_endline (term_to_string evaluated)
```

The `normalize` step limit prevents non-terminating lambda terms from running
forever. Increase it for larger encodings or more complex terms.
