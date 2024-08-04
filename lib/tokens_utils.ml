
(* val create_token_kind : string -> string token_kind *)

(* val create_token_kind : char -> char token_kind *)
(* open Camllox *)
(* let create_token_kind str = match str with 
| '>':: '=':: rest -> Greater_equal (Lexeme_string ">=")
 *)
let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let%test _ = fact 1 = 120