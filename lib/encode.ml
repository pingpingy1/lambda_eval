include Term

(* Substitute [s] for variable [j]. The beta-reduction rule below shifts the
   argument before substitution and shifts the result back afterward. *)
let rec subst (j : int) (s : term) = function
  | Var k -> if k = j then s else Var k
  | Lam body -> Lam (subst (j + 1) (shift 1 0 s) body)
  | App (t1, t2) -> App (subst j s t1, subst j s t2)

(* Small-step reduction to normal form: CBV then under function bodies. *)
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
let rec normalize (max_steps : int) (t : term) : term =
  if max_steps <= 0 then t
  else
    match eval_step t with Some t' -> normalize (max_steps - 1) t' | None -> t

(* *********************************************************************
 * Scott encoding: generally applicable encoding of data types
 * For a datatype with N constructors c_i with respective arities a_i,
 * c_i is encoded as the following:
 * \lambda x_1. \lambda x_2. ... \lambda x_{a_i}.
 *   \lambda c_1. \lambda c_2. ... \lambda c_N.
 *     c_i x_1 x_2 ... x_{a_i}
 *
 * A match expression that performs f_i (x_1, ..., x_{a_i}) for case c_i
 * is then evaluated as a f_1 f_2 ... f_N.
 *
 * Lambda calculus has three constructors:
 * - Var (arity = 1) => encode (Var n) = \lambda a b c. a (encode_nat n)
 * - Lam (arity = 1) => encode (Lam t) = \lambda a b c. b (encode t)
 * - App (arity = 2) => encode (App (t1, t2)) = \lambda a b c. c (encode t1) (encode t2)
 * *********************************************************************
 *)
let rec encode : term -> term = function
  | Var n -> Lam (Lam (Lam (App (Var 2, Nat.encode n))))
  | Lam body -> Lam (Lam (Lam (App (Var 1, encode body))))
  | App (t1, t2) -> Lam (Lam (Lam (App (App (Var 0, encode t1), encode t2))))
