open Lambda_eval.Encode
open Lambda_eval.Eval
open Lambda_eval.Utils

let rec equal t1 t2 =
  match (t1, t2) with
  | Var n1, Var n2 -> n1 = n2
  | Lam b1, Lam b2 -> equal b1 b2
  | App (f1, a1), App (f2, a2) -> equal f1 f2 && equal a1 a2
  | _ -> false

let round_trip t =
  let result = normalize 1000 (App (e_eval, encode t)) in
  let expected = normalize 1000 t in
  if not (equal result expected) then
    failwith
      ("expected " ^ term_to_string expected ^ ", got " ^ term_to_string result)

let () =
  round_trip (Lam (Var 0));
  round_trip (Lam (Lam (Var 1)));
  round_trip (App (Lam (Var 0), Lam (Var 0)));
  round_trip (Lam (App (Var 0, Var 0)))
