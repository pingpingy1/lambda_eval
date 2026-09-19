open Term
open Data

(* *********************************************************************
 * Scott encoding: generally applicable encoding of data types
 * For a datatype with N constructors c_i with respective arities a_i,
 * c_i is encoded as the following:
 * λ x_1 x_2 ... x_{a_i}.
 *   λ c_1 c_2 ... c_N.
 *     c_i x_1 x_2 ... x_{a_i}
 *
 * A match expression that performs f_i (x_1, ..., x_{a_i}) for case c_i
 * is then evaluated as t f_1 f_2 ... f_N.
 *
 * Lambda calculus has three constructors:
 * - Var (arity = 1) => encode (Var n) = λ a b c. a (encode_nat n)
 * - Lam (arity = 1) => encode (Lam t) = λ a b c. b (encode t)
 * - App (arity = 2) => encode (App (t1, t2)) = λ a b c. c (encode t1) (encode t2)
 * Application of Scott encoding to lambda terms is also called the "Mogensen-Scott encoding"
 * *********************************************************************
 *)
let rec encode : term -> term = function
  | Var n -> Lam (Lam (Lam (App (Var 2, encode_nat n))))
  | Lam body -> Lam (Lam (Lam (App (Var 1, encode body))))
  | App (t1, t2) -> Lam (Lam (Lam (App (App (Var 0, encode t1), encode t2))))

(* c_var n = λ a b c. a n *)
let c_var (n : term) : term = App (Lam (Lam (Lam (Lam (App (Var 2, Var 3))))), n)

(* c_lam t = λ a b c. b t *)
let c_lam (t : term) : term = App (Lam (Lam (Lam (Lam (App (Var 1, Var 3))))), t)

(* c_app f a = λ a b c. c f a *)
let c_app (f : term) (a : term) : term =
  App (App (Lam (Lam (Lam (Lam (Lam (App (App (Var 0, Var 4), Var 3)))))), f), a)
