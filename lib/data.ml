open Term

(* Data structures for other implementations *)

(* Church encoding of natural numbers. *)
let rec encode_nat (n : int) : term =
  if n < 0 then invalid_arg "Nat.encode: negative number"
  else if n = 0 then Lam (Lam (Var 1))
  else Lam (Lam (App (Var 0, encode_nat (n - 1))))

(* Church encoding of pairs *)
let pair (f : term) (s : term) : term =
  Lam (App (App (Var 0, shift 1 0 f), shift 1 0 s))

let lFst (p : term) : term = App (p, Lam (Lam (Var 1)))
let lSnd (p : term) : term = App (p, Lam (Lam (Var 0)))

(* Church encoding of lists. *)
(* nil = λ c n. n *)
let nil : term = Lam (Lam (Var 0))

(* cons h t = λ c n. c h (t c n) *)
let cons (h : term) (t : term) : term =
  Lam
    (Lam (App (App (Var 1, shift 2 0 h), App (App (shift 2 0 t, Var 1), Var 0))))

(* head l = l (λ h r. h) nil *)
let head (l : term) : term = App (App (l, Lam (Lam (Var 1))), nil)

(* tail l = λ c n. l (λ h r g. g h (r c)) (λ g. n) (λ h t. t) *)
let tail (l : term) : term =
  Lam
    (Lam
       (App
          ( App
              ( App
                  ( shift 2 0 l,
                    Lam
                      (Lam (Lam (App (App (Var 0, Var 2), App (Var 1, Var 4)))))
                  ),
                Lam (Var 1) ),
            Lam (Lam (Var 0)) )))
