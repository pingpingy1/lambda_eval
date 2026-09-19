open Tokens

(* Tokiziser for human inputs in string format *)
let is_digit : char -> bool = function '0' .. '9' -> true | _ -> false

let is_white : char -> bool = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let tokenize (s : string) : tok list =
  let len = String.length s in
  let rec read_nat (acc : int) (idx : int) : int * int =
    if idx < len && is_digit s.[idx] then
      let digit = Char.code s.[idx] - Char.code '0' in
      read_nat ((acc * 10) + digit) (idx + 1)
    else (acc, idx)
  in
  let rec aux (idx : int) (acc : tok list) : tok list =
    if idx >= len then List.rev acc
    else
      match s.[idx] with
      | c when is_white c -> aux (idx + 1) acc
      | '(' -> aux (idx + 1) (T_lpar :: acc)
      | ')' -> aux (idx + 1) (T_rpar :: acc)
      | '\\' when idx + 1 < len && s.[idx + 1] = '.' ->
          aux (idx + 2) (T_lam :: acc)
      (* λ = 0xCE 0xBB *)
      | '\xCE' when idx + 2 < len && s.[idx + 1] = '\xBB' && s.[idx + 2] = '.'
        ->
          aux (idx + 3) (T_lam :: acc)
      | c when is_digit c ->
          let n, idx' = read_nat 0 idx in
          aux idx' (T_nat n :: acc)
      | c ->
          failwith
            (Printf.sprintf "Lexer error: unexpected character '%c' at index %d"
               c idx)
  in
  aux 0 []
