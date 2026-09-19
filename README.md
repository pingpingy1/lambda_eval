# Lambda AST Encoding and Evaluation

A self-interpreter for untyped $\lambda$-calculus written in OCaml, comprising:
1. **De Bruijn Representation:** Canonical representations of $\lambda$-terms without variable capture.
2. **Scott Encodings:** Algebraic data types (terms, lists, naturals, pairs, and tokens) mapped directly into closed $\lambda$-combinators.
3. **Internal Parser Combinator (`e_parse`):** A recursive-descent parser defined **inside pure $\lambda$-calculus** that consumes Scott-encoded token streams and constructs the syntax tree combinator.
4. **Self-Interpreter (`e_eval`):** A self-evaluator $E$ that executes an AST representation within the calculus itself ($E \, \ulcorner M \urcorner \rightarrow_\beta M$).
5. **Interactive REPL:** A front-end interface for evaluating expressions typed in keyboard-friendly notation.

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
├── data.ml                 Useful data structures: naturals, pairs, lists
├── tokens.ml               Tokens and their Scott encoding
├── encode.ml               Mogensen-Scott AST encoding
├── eval.ml                 Beta reduction and encoded evaluator
└── lexer.ml                Lexer for user inputs
```

## Building and running

```sh
dune build
dune exec lambda_eval
```

This executes a parse&evaluate example, and starts a REPL that takes user-input programs.
You may use either λ or a backslash \ for function definition.
***You must parenthesize every subexpression***, as the parser is very strict in its grammar.

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
