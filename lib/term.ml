(* Untyped lambda expressions using de Bruijn indices. *)
type term = Var of int | Lam of term | App of term * term

(* Raise variable indices greater than or equal to [c] by [d]. *)
let rec shift (d : int) (c : int) = function
  | Var k -> if k < c then Var k else Var (k + d)
  | Lam body -> Lam (shift d (c + 1) body)
  | App (t1, t2) -> App (shift d c t1, shift d c t2)
