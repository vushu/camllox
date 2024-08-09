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

 let rec skip_until_newline chars =
  match chars with
  | [] -> []  (* Reached the end of the list *)
  | '\n' :: rest -> rest  (* Found newline, return the rest of the list *)
  | _ :: rest -> skip_until_newline rest  (* Continue skipping *)


let chars_as_string    f s = String.make 1 f ^ String.make 1 s

let chars_to_string l = String.of_seq (List.to_seq l)

let rec extract_string_from_rest acc chars line = 
  match chars with
  | [] -> (acc, chars, line)
  | '"' :: rest -> (acc, rest, line)
  | '\n' :: rest -> extract_string_from_rest acc rest (line + 1)
  | x :: rest -> extract_string_from_rest (acc @ [x]) rest line

let string_to_char_list s =
  List.of_seq (String.to_seq s)
let mama: token list = [Token (Left_paren '(', None , 1)]

let look_up_keyword word = match word with
  | "and" -> Some(And("and"))
  | "class" -> Some(Class("class"))
  | "else" -> Some(Else("else"))
  | "false" -> Some(False("false"))
  | "for" -> Some(For("for"))
  | "fun" -> Some(Fun("fun"))
  | "if" -> Some(If("if"))
  | "nil" -> Some(Nil("nil"))
  | "or" -> Some(Or("or"))
  | "print" ->Some( Print("print"))
  | "return" -> Some(Return("return"))
  | "super" -> Some(Super("super"))
  | "this" -> Some(This("this"))
  | "true" -> Some(True("true"))
  | "var" -> Some(Var("var"))
  | "while" -> Some(While("while"))
  | _ -> None

let%test _ = let t = Token(EOF(),None, 1) in
  match t with
  | Token (k, _ , _) -> (match k with | EOF() -> true
    | _ -> false)
  


(* let%test _ = let t = Float_Token(LParen ['('], Float_literal 42.0 , 1) and
  _t2 = String_Token(LParen ['*'], String_literal "ss", 1) in
  match t with
  | Float_Token (_kind, lit, _line) -> (match lit with | Float_literal x -> x = 42.0 )
    (* (match kind with | LParen (l::_) -> l = '(' | _ -> false) *)

  | _ -> false *)