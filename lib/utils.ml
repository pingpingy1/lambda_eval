open Term

let rec term_to_string : term -> string = function
  | Var n -> string_of_int n
  | Lam body -> "(λ. " ^ term_to_string body ^ ")"
  | App (fn, arg) -> "(" ^ term_to_string fn ^ " " ^ term_to_string arg ^ ")"
