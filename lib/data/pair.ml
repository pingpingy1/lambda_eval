open Term

(* Church encoding of pairs *)
let pair (f : term) (s : term) : term = Lam (App (App (Var 0, shift 1 0 f), shift 1 0 s))
let fst (p : term) : term = App (p, Lam (Lam (Var 1)))
let snd (p : term) : term = App (p, Lam (Lam (Var 0)))
