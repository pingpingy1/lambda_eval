open Lambda_eval.Term
open Lambda_eval.Encode
open Lambda_eval.Eval
open Lambda_eval.Tokens
open Lambda_eval.Lexer

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
  let eval_repr : term =
    match normalize 1000 (App (e_eval, repr)) with
    | Normal t -> t
    | Timeout t ->
        print_endline "Warning: Timeout after 1000 steps";
        t
  in
  print_endline ("Original:       " ^ term_to_string t);
  print_endline ("Representation: " ^ term_to_string repr);
  print_endline ("Evaluated:      " ^ term_to_string eval_repr);
  print_endline ""

let _ = print_description
let _ = test_term (Lam (Var 0))
let _ = test_term (App (Lam (Var 0), Lam (Var 0)))

let toks =
  encode_toks
    [
      T_lpar;
      T_lpar;
      T_lam;
      T_nat 0;
      T_rpar;
      T_lpar;
      T_lam;
      T_nat 0;
      T_rpar;
      T_rpar;
    ]

let _ = print_endline ("Encoded tokens: " ^ term_to_string toks)
let ast = App (e_parse, toks) |> normalize_get 10000
let _ = print_endline ("AST: " ^ term_to_string ast)
let res = App (e_eval, ast) |> normalize_get 1000
let _ = print_endline ("Evaluation: " ^ term_to_string res)
let _ = print_endline "Try it yourself!"
let s = read_line ()
let toks = tokenize s |> encode_toks
let _ = print_endline ("Encoded tokens: " ^ term_to_string toks)
let ast = App (e_parse, toks) |> normalize_get 10000
let _ = print_endline ("AST: " ^ term_to_string ast)
let res = App (e_eval, ast) |> normalize_get 1000
let _ = print_endline ("Evaluation: " ^ term_to_string res)
