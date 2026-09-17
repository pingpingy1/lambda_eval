open Lambda_eval.Encode
open Lambda_eval.Eval
open Lambda_eval.Utils

let print_description : unit =
  print_endline
    "This project takes the abstract syntax tree (AST) of a lambda term,";
  print_endline
    "represents it as a separate lambda term (so-called Morgensen-Scott";
  print_endline "encoding), and evaluates into the term it encodes.";
  print_endline "The evaluator is given by:";
  print_endline (term_to_string e_eval);
  print_endline ""

let test_term (t : term) : unit =
  let repr : term = encode t in
  let eval_repr : term = normalize 1000 (App (e_eval, repr)) in
  print_endline ("Original:       " ^ term_to_string t);
  print_endline ("Representation: " ^ term_to_string repr);
  print_endline ("Evaluated:      " ^ term_to_string eval_repr);
  print_endline ""

let _ = print_description
let _ = test_term (Lam (Var 0))
let _ = test_term (App (Lam (Var 0), Lam (Var 0)))
