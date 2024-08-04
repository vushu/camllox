type _ token_kind =
  | Left_paren : char -> char token_kind
  | Right_paren : char -> char token_kind
  | Left_brace : char -> char token_kind
  | Right_brace : char -> char token_kind
  | Comma : char -> char token_kind
  | Dot : char -> char token_kind
  | Minus : char -> char token_kind
  | Plus : char -> char token_kind
  | Semicolon : char -> char token_kind
  | Slash : char -> char token_kind
  | Star : char -> char token_kind
  | Bang : char -> char token_kind
  | Bang_equal : string -> string token_kind
  | Equal : char -> char token_kind
  | Equal_equal : string -> string token_kind
  | Greater_equal : string -> string token_kind
  | Greater : char -> char token_kind
  | Less : char -> char token_kind
  | Less_equal : string -> string token_kind

  (* Literals *)
  | Identifier : string -> string token_kind
  | String_t : string -> string token_kind
  | Number : string -> string token_kind

  (* Keywords *)
  | And : string -> string token_kind
  | Class : string -> string token_kind
  | Else : string -> string token_kind
  | False : string -> string token_kind
  | Fun : string -> string token_kind
  | For : string -> string token_kind
  | If : string -> string token_kind
  | Nil : string -> string token_kind
  | Or : string -> string token_kind
  | Print : string -> string token_kind
  | Return : string -> string token_kind
  | Super : string -> string token_kind
  | This : string -> string token_kind
  | True : string -> string token_kind
  | Var : string -> string token_kind
  | While : string -> string token_kind
  | EOF : unit -> unit token_kind


type _ literal =
  | Float_literal : float -> float literal
  | Int_literal : int -> int literal
  | Bool_literal : bool -> bool literal
  | String_literal : string -> string literal


type token = Token : ('a token_kind * 'b literal option * int) ->  token

 (* 
  | Bang_equal : string -> string token_kind
  | Equal : char -> char token_kind
  | Equal_equal : string -> string token_kind
  | Greater_equal : string -> string token_kind
  | Less : char -> char token_kind
  | Less_equal : string -> string token_kind
 *)

 let rec skip_until_newline chars =
  match chars with
  | [] -> []  (* Reached the end of the list *)
  | '\n' :: rest -> rest  (* Found newline, return the rest of the list *)
  | _ :: rest -> skip_until_newline rest  (* Continue skipping *)


let chars_as_string    f s = String.make 1 f ^ String.make 1 s

let rec handle_string acc chars line = 
  match chars with
  | [] -> (acc, chars, line)
  | '"' :: rest -> (acc, rest, line)
  | '\n' :: rest -> handle_string acc rest (line + 1)
  | x :: rest -> handle_string (acc @ [x]) rest line

let string_to_char_list s =
  List.of_seq (String.to_seq s)
let mama: token list = [Token (Left_paren '(', None , 1)]

let%test _ = 
  let rec scan code line = match code with
  | [] -> [Token(EOF(), None, line)]
  | '(' as c :: rest -> [Token(Left_paren c, None, line)] @ scan rest line 
  | ')' as c :: rest -> [Token(Right_paren c, None, line)] @ scan rest line 
  | '{' as c :: rest -> [Token(Left_brace c, None, line)] @ scan rest line
  | '}' as c :: rest -> [Token(Right_brace c, None, line)] @ scan rest line 
  | ',' as c :: rest -> [Token(Comma c, None, line)] @ scan rest line 
  | '.' as c :: rest -> [Token(Dot c, None, line)] @ scan rest line 
  | '-' as c :: rest -> [Token(Minus c, None, line)] @ scan rest line 
  | '+' as c :: rest -> [Token(Plus c, None, line)] @ scan rest line 
  | ';' as c :: rest -> [Token(Semicolon c, None, line)] @ scan rest line 
  | '*' as c :: rest -> [Token(Star c, None, line)] @ scan rest line 
  | ('!' as f) :: ('=' as s) :: rest  -> [Token(Bang_equal (chars_as_string f s), None, line)] @ scan rest line 
  | '!' as c :: rest -> [Token(Bang c, None, line)] @ scan rest line 
  | ('=' as f) :: ('=' as s) :: rest  -> [Token(Equal_equal (chars_as_string f s), None, line)] @ scan rest line 
  | '=' as c :: rest -> [Token(Equal c, None, line)] @ scan rest line 
  | ('>' as f) :: ('=' as s) :: rest  -> [Token(Greater_equal (chars_as_string f s), None, line)] @ scan rest line 
  | '>' as c :: rest -> [Token(Greater c, None, line)] @ scan rest line 
  | ('<' as f) :: ('=' as s) :: rest  -> [Token(Less_equal (chars_as_string f s), None, line)] @ scan rest line 
  | '<' as c :: rest -> [Token(Less c, None, line)] @ scan rest line 
  | '/' :: '/' :: rest -> scan (skip_until_newline rest) line
  | '/' as c :: rest -> [Token(Slash c, None, line )] @ scan rest line
  | (' ' | '\r' | '\t'):: rest -> scan rest line
  |  '\n' :: rest -> scan rest (line + 1)
  |  '"' :: rest -> scan rest (line + 1)
  | _ -> [] in

  let res : token list = (scan (string_to_char_list "!=)/") 1) in
  (List.length res) = 4

  
(* let toks : token list = [Float_token (2., (Greater_equal "sdfsdfa"), 1)] *)

let%test _ = let (acc, _rest, _line ) = handle_string [] ['a';'b'; 'c';'"'; 'b'] 1 in
  acc = ['a'; 'b'; 'c']

(* let%test _ = let t = Float_Token(LParen ['('], Float_literal 42.0 , 1) and
  _t2 = String_Token(LParen ['*'], String_literal "ss", 1) in
  match t with
  | Float_Token (_kind, lit, _line) -> (match lit with | Float_literal x -> x = 42.0 )
    (* (match kind with | LParen (l::_) -> l = '(' | _ -> false) *)

  | _ -> false *)