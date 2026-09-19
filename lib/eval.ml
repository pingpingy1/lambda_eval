open Term
open Data

(* **************************************
 * When evaluating a lambda representation,
 * we need to keep track of variables that have been defined thus far.
 * We use a list of natural numbers, again represented in lambda calculus.
 *
 * Evaluation of such an encoding consists of three parts:
 * - Variable lookup: Fetch the n-th variable in that list
 * - Function construction: Prepend the list with the new variable and evaluate the body
 * - Application: Recursively evaluate the function and the argument
 * **************************************
 *)

(* lookup lst n = n (head lst) (λ n'. lookup (tail lst) n') *)
let lookup : term =
  App
    ( y_comb,
      Lam
        (Lam
           (Lam
              (App
                 ( App (Var 0, head (Var 1)),
                   Lam (App (App (Var 3, tail (Var 2)), Var 0)) )))) )

(* This inductive evaluation combinator should satisfy:
 * eval_comb t nil ->* (normal form of t) *)
let eval_comb : term =
  App
    ( y_comb,
      Lam
        (Lam
           (Lam
              ((* v_case lst (Var n) = lookup lst n *)
               let v_case = Lam (App (App (lookup, Var 1), Var 0)) in
               (* l_case lst (Lam b) = Lam (eval b ((Var 0) :: lst)) *)
               let l_case =
                 Lam (Lam (App (App (Var 4, Var 1), cons (Var 0) (Var 2))))
               in
               (* a_case lst (App (t1, t2)) = App (eval t1 lst) (eval t2 lst) *)
               let a_case =
                 Lam
                   (Lam
                      (App
                         ( App (App (Var 4, Var 1), Var 2),
                           App (App (Var 4, Var 0), Var 2) )))
               in
               App (App (App (Var 1, v_case), l_case), a_case)))) )

(* eval = λ t. eval_comb t nil *)
let e_eval : term = Lam (App (App (eval_comb, Var 0), nil))
