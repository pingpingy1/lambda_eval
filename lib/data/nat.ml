open Term

(* Church encoding of natural numbers. *)
let rec encode_nat (n : int) : term =
  if n < 0 then invalid_arg "Nat.encode: negative number"
  else if n = 0 then Lam (Lam (Var 1))
  else Lam (Lam (App (Var 0, encode_nat (n - 1))))
