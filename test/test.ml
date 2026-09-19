open Lambda_eval

let rec equal t1 t2 =
  match (t1, t2) with
  | Var n1, Var n2 -> n1 = n2
  | Lam b1, Lam b2 -> equal b1 b2
  | App (f1, a1), App (f2, a2) -> equal f1 f2 && equal a1 a2
  | _ -> false

let three_way_trip name t =
    let direct = t |> normalize_get 1000 in
    let ast_eval = t |> encode |> normalize_get 1000 |> (fun x -> App (e_eval, x)) |> normalize_get 1000 in
    let tok_ast_eval = t |> term_to_string |> tokenize |> encode_toks |> normalize_get 1000 |> (fun x -> App (e_parse, x)) |> normalize_get 1000 |> (fun x -> App (e_eval, x)) |> normalize_get 1000 in
    if equal direct ast_eval && equal direct tok_ast_eval then
      Printf.printf "Test %s success!\n" name
    else
      failwith (Printf.sprintf "Test %s fail:\nDirect: %s\nAST evaluation: %s \nTokens->AST evaluation: %s" name (term_to_string direct) (term_to_string ast_eval) (term_to_string tok_ast_eval))

let parser_cases =
  [
    ("abstraction", Lam (Var 0));
    ( "nested application",
      App (Lam (Var 0), Lam (Var 0)) );
    ("true", Lam (Lam (Var 1)));
  ]

let _ = List.iter
    (fun (name, term) -> three_way_trip name term)
    parser_cases
