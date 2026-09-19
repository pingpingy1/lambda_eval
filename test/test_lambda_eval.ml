open Lambda_eval.Encode
open Lambda_eval.Eval
open Lambda_eval.Lambda_list
open Lambda_eval.Tokens
open Lambda_eval.Utils

let rec equal t1 t2 =
  match (t1, t2) with
  | Var n1, Var n2 -> n1 = n2
  | Lam b1, Lam b2 -> equal b1 b2
  | App (f1, a1), App (f2, a2) -> equal f1 f2 && equal a1 a2
  | _ -> false

let round_trip name t =
  let result = normalize 1000 (App (e_eval, encode t)) in
  let expected = normalize 1000 t in
  if equal result expected then print_endline ("Test: " ^ name ^ " success!")
  else
    failwith
      ("expected " ^ term_to_string expected ^ ", got " ^ term_to_string result)

let pair_fst = Lam (App (Var 0, Lam (Lam (Var 1))))

let evaluate_parsed_tokens tokens =
  let parsed = App (e_parse, encode_toks tokens) in
  let encoded_ast = normalize 10000 (App (pair_fst, parsed)) in
  normalize 10000 (App (e_eval, encoded_ast))

let compare_evaluations name term tokens =
  let expected = normalize 10000 term in
  let encoded = normalize 10000 (App (e_eval, encode term)) in
  let parsed = evaluate_parsed_tokens tokens in
  if equal expected encoded && equal encoded parsed then
    print_endline ("Test: " ^ name ^ " success!")
  else
    failwith
      ("evaluation mismatch for " ^ name ^ "\n" ^ "normal form: "
     ^ term_to_string expected ^ "\n" ^ "encoded AST: " ^ term_to_string encoded
     ^ "\n" ^ "parsed AST: " ^ term_to_string parsed)

let list_laws () =
  let values = cons (Var 10) (cons (Var 20) nil) in
  let first = normalize 1000 (head values) in
  let second = normalize 1000 (head (tail values)) in
  if not (equal first (Var 10)) then failwith "head/cons law failed";
  if not (equal second (Var 20)) then failwith "head/tail law failed"

let parser_cases =
  [
    ("abstraction", Lam (Var 0), [ T_lpar; T_lam; T_nat 0; T_rpar ]);
    ( "nested application",
      App (Lam (Var 0), Lam (Var 0)),
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
      ] );
  ]

let () =
  round_trip "identity" (Lam (Var 0));
  round_trip "true" (Lam (Lam (Var 1)));
  round_trip "identity of identity" (App (Lam (Var 0), Lam (Var 0)));
  round_trip "self application" (Lam (App (Var 0, Var 0)));
  list_laws ();
  List.iter
    (fun (name, term, tokens) -> compare_evaluations name term tokens)
    parser_cases
