 type _ literal =
  | Int_literal : int -> int literal
  | Bool_literal : bool -> bool literal
  | Float_literal : float -> float literal
  | String_literal : string -> string literal 

type _ lexeme =
  | Lexeme_char : char -> char lexeme
  | Lexeme_string : string -> string lexeme

(* type _ token_kind = 
  | Left_paren : 'a lexeme -> 'a token_kind
  | Right_paren : 'a lexeme -> 'a token_kind
  | Greater_equal : 'a lexeme -> 'a token_kind
  | Less_equal : 'a lexeme -> 'a token_kind *)
(* val create_token_kind : string -> string token_kind *)
(* val create_token_kind : char -> char token_kind *)
type _ token_type = 
  | Left_paren : char -> char token_type
  | Right_paren : char -> char token_type
  | Greater_equal : string -> string token_type
  | Less_equal : string -> string token_type


type ('a, 'b) token = 
| Token : 'a token_type * 'b literal -> ('a ,'b) token


type tokeny =
 | D_number_token of (string, float) token
 | Single_number_ of (char, float) token





(* val greater_equal : string token_kind
val make_token : 'a token_kind -> string -> 'a token *)
let rec fact n = if n = 1 then 1 else n * fact (n - 1)
(* 
let print_literal literal = match literal with 
 | Int_literal i -> string_of_int i
 | String_literal s -> string_of
 | Bool_literal b -> string_of_bool b
 *)
let extract_int l = match l with
 | Int_literal x -> x



let%test _ = fact 5 = 120

let%test _ = let l = Left_paren ('}') in
  match l with
  | Left_paren x -> x = '}'
  | _ -> false



let%test _ = let t = Token ((Left_paren '('),  (Int_literal 42)) in
match t with 
| Token (_tk, li) -> (match li with | Int_literal v -> v = 42)


let%test _ = let t = Token  ((Left_paren '(', Int_literal 42)) in
match t with 
| Token (_tk, li) -> (match li with | Int_literal v -> v = 42)

(* let%test _ = 
  let t = Token  ((Left_paren '(', Int_literal 42)) and
      t2 = Token  ((Right_paren '(', Float_literal 42.)) and 
      t3 = Token  ((Greater_equal ">=", Bool_literal true)) and 
      t4 = Token  ((Greater_equal ">=", Bool_literal true)) in
      true *)
  (* let l = [t; t2; t3] in *)
  (* let i = List. t4 l in *)
  (* List.length l = 4 *)



