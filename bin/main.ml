open Lambda_eval

let print_description () : unit =
  print_endline "=================================================";
  print_endline " Lambda Calculus Self-Interpreter & Parser REPL  ";
  print_endline " Syntax: t ::= n | (λ. t) or (\\. t) | (t1 t2)   ";
  print_endline " Commands: :q / :quit to exit                    ";
  print_endline "=================================================";
  print_endline "";
  print_endline "Example:"

let test_term (t : term) : unit =
  print_endline ("Original: " ^ term_to_string t);
  let toks : tok list = t |> term_to_string |> tokenize in
  print_endline ("Tokens: " ^ string_of_toks toks);
  let toks_repr : term = encode_toks toks |> normalize_get 1000 in
  print_endline ("Token representation: " ^ term_to_string toks_repr);
  let ast_from_tok : term = normalize_get 1000 (App (e_parse, toks_repr)) in
  print_endline ("AST representation: " ^ term_to_string ast_from_tok);
  print_endline ("<-> Direct AST: " ^ (t |> encode |> term_to_string));
  let result : term = App (e_eval, ast_from_tok) |> normalize_get 1000 in
  print_endline ("Evaluated: " ^ term_to_string result);
  print_endline ""

let _ = print_description ()
let _ = test_term (App (Lam (Var 0), Lam (Var 0)))

let _ = print_endline "Try it yourself!"

let rec loop () : unit =
  let _ = print_string "> " in
  let _ = flush stdout in
  let s = read_line () |> String.trim in
  if s = ":q" || s = ":quit" then print_endline "Bye~" else
  if s = "" then loop () else
  let toks = tokenize s |> encode_toks in
  let _ = print_endline ("Encoded tokens: " ^ term_to_string toks) in
  let ast = App (e_parse, toks) |> normalize_get 10000 in
  let _ = print_endline ("AST: " ^ term_to_string ast) in
  let res = App (e_eval, ast) |> normalize_get 1000 in
  let _ = print_endline ("Evaluation: " ^ term_to_string res) in
  loop ()

let _ = loop ()
