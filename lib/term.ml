(* Untyped lambda expressions using de Bruijn indices. *)
type term = Var of int | Lam of term | App of term * term

(* Raise variable indices greater than or equal to [c] by [d]. *)
let rec shift (d : int) (c : int) = function
  | Var k -> if k < c then Var k else Var (k + d)
  | Lam body -> Lam (shift d (c + 1) body)
  | App (t1, t2) -> App (shift d c t1, shift d c t2)

let rec term_to_string : term -> string = function
  | Var n -> string_of_int n
  | Lam body -> "(λ. " ^ term_to_string body ^ ")"
  | App (fn, arg) -> "(" ^ term_to_string fn ^ " " ^ term_to_string arg ^ ")"

(* Y-combinator: Y = λ f. (λ x. f (x x)) (λ x. f(x x)) *)
let y_comb =
  Lam
    (App
       ( Lam (App (Var 1, App (Var 0, Var 0))),
         Lam (App (Var 1, App (Var 0, Var 0))) ))

(* Substitute [s] for variable [j]. The beta-reduction rule below shifts the
   argument before substitution and shifts the result back afterward. *)
let rec subst (j : int) (s : term) = function
  | Var k -> if k = j then s else Var k
  | Lam body -> Lam (subst (j + 1) (shift 1 0 s) body)
  | App (t1, t2) -> App (subst j s t1, subst j s t2)

(* Small-step reduction to normal form: CBN then under function bodies. *)
let rec eval_step : term -> term option = function
  | Var _ -> None
  | Lam body -> (
      match eval_step body with Some body' -> Some (Lam body') | None -> None)
  | App (Lam body, arg) -> Some (shift (-1) 0 (subst 0 (shift 1 0 arg) body))
  | App (t1, t2) -> (
      match eval_step t1 with
      | Some t1' -> Some (App (t1', t2))
      | None -> (
          match eval_step t2 with
          | Some t2' -> Some (App (t1, t2'))
          | None -> None))

(* Fully normalize within a given number of steps *)
type normal_form = Normal of term | Timeout of term

let rec normalize (max_steps : int) (t : term) : normal_form =
  if max_steps <= 0 then Timeout t
  else
    match eval_step t with
    | Some t' -> normalize (max_steps - 1) t'
    | None -> Normal t

let rec normalize_get (max_steps : int) (t : term) : term =
  match normalize max_steps t with
  | Normal nf -> nf
  | Timeout nf ->
      print_endline
        ("WARNING: Normalization timeout after " ^ string_of_int max_steps
       ^ " steps");
      nf
