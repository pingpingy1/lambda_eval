open Term
open Data
open Pair
open Encode

(* ***********************************************
 * We define the following tokens for the concrete syntax of lambda terms:
 * Tok ::= T_lpar | T_rpar | T_lam | T_nat of n
 *
 * Under the Scott encoding scheme, these can be encoded as:
 * encode T_lpar    := λ a b c d. a
 * encode T_rpar    := λ a b c d. b
 * encode T_lam     := λ a b c d. c
 * encode (T_nat n) := λ a b c d. d (encode_nat n)
 * ***********************************************
 *)
type tok = T_lpar | T_rpar | T_lam | T_nat of int

let string_of_tok : tok -> string = function
  | T_lpar -> "T_LPAR"
  | T_rpar -> "T_RPAR"
  | T_lam -> "T_LAM"
  | T_nat n -> Printf.sprintf "T_NAT(%d)" n

let string_of_toks (toks : tok list) : string =
  Printf.sprintf "[%s]" (String.concat "; " (List.map string_of_tok toks))

let encode_tok : tok -> term = function
  | T_lpar -> Lam (Lam (Lam (Lam (Var 3))))
  | T_rpar -> Lam (Lam (Lam (Lam (Var 2))))
  | T_lam -> Lam (Lam (Lam (Lam (Var 1))))
  | T_nat n -> Lam (Lam (Lam (Lam (App (Var 0, encode_nat n)))))

let rec encode_toks : tok list -> term = function
  | [] -> nil
  | t :: lst -> cons (encode_tok t) (encode_toks lst)

(* ***********************************************
 * We implement a top-down recursive parser:
 * parse nil = nil
 * parse ((T_nat n) :: lst) = (Var n, lst)
 * parse (T_lpar :: T_lam :: lst) =
 *   let parse lst = (t, T_rpar :: lst') in
 *   (Lam t, lst')
 * parse (T_lpar :: lst) =
 *   let parse lst = (t1, T_rpar :: lst') in
 *   let parse lst' = (t2, T_rpar :: lst'') in
 *   (App (t1, t2), lst'')
 * ***********************************************
 *)

(* ***********************************************
 * NOTE on list matching. Lambda_list uses the Church (fold) encoding, so
 *   l (λ h r. body) nil
 * binds [r] to the *fold of the tail* (t c nil), NOT to the tail itself.
 * To recover the tail we use [tail l] on the enclosing list variable and
 * ignore [r], e.g.  toks (λ tok1 _. ... (tail toks) ...) nil.
 * ***********************************************
 *)

(* lam_case = λ self rest.
 *   (λ res. pair (c_lam (fst res)) (tail (snd res)))
 *   (self rest)
 *)
let lam_case : term =
  Lam
    ((* self *)
       Lam
       ((* rest *)
          App
          ( (* λ res. pair (c_lam (fst res)) (tail (snd res)) *)
            Lam (pair (c_lam (lFst (Var 0))) (tail (lSnd (Var 0)))),
            (* self rest *)
            App (Var 1, Var 0) )))

(* app_case = λ self rest.
 *   (λ res1.
 *     (λ rest'.
 *       (λ res2.
 *         pair (c_app (fst res1) (fst res2)) (tail (snd res2))
 *       ) (self rest')
 *     ) (snd res1)
 *   ) (self rest) *)
let app_case : term =
  Lam
    ((* self *)
       Lam
       ((* rest *)
          App
          ( Lam
              ((* res1 *)
                 App
                 ( Lam
                     ((* rest' *)
                        App
                        ( Lam
                            ((* res2 *)
                             pair
                               (c_app (lFst (Var 2)) (lFst (Var 0)))
                               (tail (lSnd (Var 0)))),
                          App (Var 3, Var 0) (* (self rest') *) )),
                   lSnd (Var 0) (* (snd res1) *) )),
            App (Var 1, Var 0) (* (self rest) *) )))

(* lpar_case = λ self rest1.
 *   rest1
 *     (λ tok2 _.
 *       tok2
 *         (app_case self rest1)
 *         nil                      <-- Unexpected T_rpar!
 *         (lam_case self (tail rest1))
 *         (λ n. app_case self rest1)
 *     ) nil
 *)
let lpar_case : term =
  Lam
    ((* self *)
       Lam
       ((* rest1 *)
          App
          ( App
              ( Var 0,
                (* rest1 *)
                Lam
                  ((* tok2 *)
                     Lam
                     ((* rest2 *)
                        App
                        ( App
                            ( App
                                ( App
                                    ( Var 1,
                                      (* tok2 *)
                                      App (App (app_case, Var 3), Var 2)
                                      (* app_case self rest1 *) ),
                                  nil ),
                              App (App (lam_case, Var 3), tail (Var 2))
                              (* lam_case self (tail rest1) *) ),
                          Lam
                            ((* n *)
                               App
                               (App (app_case, Var 4), Var 3)) ))) ),
            nil )))

(* parse_arch = λ self toks.
 *  toks
 *    (λ tok1 _.
 *      tok1
 *        (lpar_case self (tail toks))
 *        nil
 *        nil
 *        (λ n. pair (c_var n) (tail toks))
 *    ) nil
 *)
let parse_arch : term =
  Lam
    ((* self *)
       Lam
       ((* toks *)
          App
          ( App
              ( Var 0,
                (* toks *)
                Lam
                  ((* tok1 *)
                     Lam
                     ((* rest1 *)
                        App
                        ( App
                            ( App
                                ( App
                                    ( Var 1,
                                      (* tok1 *)
                                      App (App (lpar_case, Var 3), tail (Var 2))
                                      (* lpar_case self (tail toks) *) ),
                                  nil ),
                              nil ),
                          Lam
                            ((* n *)
                             pair (c_var (Var 0))
                               (tail (Var 3)) (* pair (c_var n) (tail toks) *))
                        ))) ),
            nil )))

let e_parse : term = Lam (lFst (App (App (y_comb, parse_arch), Var 0)))
