open Term

(* Church encoding of lists. *)
let nil : term = Lam (Lam (Var 0))

let cons (h : term) (t : term) : term =
  Lam
    (Lam (App (App (Var 1, shift 2 0 h), App (App (shift 2 0 t, Var 1), Var 0))))

let head (l : term) : term = App (App (l, Lam (Lam (Var 1))), nil)

let tail (l : term) : term =
  Lam
    (Lam
       (App
          ( App
              ( App
                  ( shift 2 0 l,
                    Lam
                      (Lam (Lam (App (App (Var 0, Var 2), App (Var 1, Var 5)))))
                  ),
                Lam (Var 1) ),
            Lam (Lam (Var 0)) )))
