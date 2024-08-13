type token_kind =
  | Left_paren
  | Right_paren
  | Left_brace
  | Right_brace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | Bang_equal
  | Equal
  | Equal_equal
  | Greater_equal
  | Greater
  | Less
  | Less_equal
  (* Literals *)
  | Identifier of string
  | String_t of string
  | Number of float
  (* Keywords *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | EOF
[@@deriving show]

(* type token = Token : (token_kind * int) -> token *)
type token = token_kind * int

let rec skip_until_newline chars =
  match chars with
  | [] -> [] (* Reached the end of the list *)
  | '\n' :: rest -> rest (* Found newline, return the rest of the list *)
  | _ :: rest -> skip_until_newline rest (* Continue skipping *)

let chars_as_string f s = String.make 1 f ^ String.make 1 s
let chars_to_string l = String.of_seq (List.to_seq l)

let rec extract_string_from_rest acc chars line =
  match chars with
  | [] -> (acc, chars, line)
  | '"' :: rest -> (acc, rest, line)
  | '\n' :: rest -> extract_string_from_rest acc rest (line + 1)
  | x :: rest -> extract_string_from_rest (acc @ [ x ]) rest line

let string_to_char_list s = List.of_seq (String.to_seq s)

let look_up_keyword word =
  match word with
  | "and" -> Some And
  | "class" -> Some Class
  | "else" -> Some Else
  | "false" -> Some False
  | "for" -> Some For
  | "fun" -> Some Fun
  | "if" -> Some If
  | "nil" -> Some Nil
  | "or" -> Some Or
  | "print" -> Some Print
  | "return" -> Some Return
  | "super" -> Some Super
  | "this" -> Some This
  | "true" -> Some True
  | "var" -> Some Var
  | "while" -> Some While
  | _ -> None

let token_to_string = function
  | Left_paren -> "Left_paren"
  | Right_paren -> "Right_paren"
  | Left_brace -> "Left_brace"
  | Right_brace -> "Right_brace"
  | Comma -> "Comma"
  | Dot -> "Dot"
  | Minus -> "Minus"
  | Plus -> "Plus"
  | Semicolon -> "Semicolon"
  | Slash -> "Slash"
  | Star -> "Star"
  | Bang -> "Bang"
  | Bang_equal -> "Bang_equal"
  | Equal -> "Equal"
  | Equal_equal -> "Equal_equal"
  | Greater_equal -> "Greater_equal"
  | Greater -> "Greater"
  | Less -> "Less"
  | Less_equal -> "Less_equal"
  (* Literals *)
  | Identifier x -> "Identifier: " ^ x
  | String_t x -> "String_t: " ^ x
  | Number x -> "Number: " ^ string_of_float x
  (* Keywords *)
  | And -> "And"
  | Class -> "Class"
  | Else -> "Else"
  | False -> "False"
  | Fun -> "Fun"
  | For -> "For"
  | If -> "If"
  | Nil -> "Nil"
  | Or -> "Or"
  | Print -> "Print"
  | Return -> "Return"
  | Super -> "Super"
  | This -> "This"
  | True -> "True"
  | Var -> "Var"
  | While -> "While"
  | EOF -> "EOF"
