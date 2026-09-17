open Lambda_eval.Encode
open Lambda_eval.Eval
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

let () =
  round_trip "identity" (Lam (Var 0));
  round_trip "true" (Lam (Lam (Var 1)));
  round_trip "identity of identity" (App (Lam (Var 0), Lam (Var 0)));
  round_trip "self application" (Lam (App (Var 0, Var 0)))
