open Lambda_eval.Encode
open Lambda_eval.Eval
open Lambda_eval.Utils

let test_term (t : term) : unit =
  let repr : term = encode t in
  let eval_repr : term = normalize 1000 (App (e_eval, repr)) in
  print_endline ("Original:       " ^ term_to_string t);
  print_endline ("Representation: " ^ term_to_string repr);
  print_endline ("Evaluated:      " ^ term_to_string eval_repr);
  print_endline ""

let _ = test_term (Lam (Var 0))
let _ = test_term (App (Lam (Var 0), Lam (Var 0)))
