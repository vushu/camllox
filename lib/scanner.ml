open Tokens

let eof line = [Token(EOF(), None, line)]
let left_paren line = [Token(Left_paren '(', None, line)]
let right_paren line = [Token(Right_paren ')', None, line)]
let left_brace line = [Token(Left_brace '{', None, line)]
let right_brace line = [Token(Right_brace '}', None, line)]
let comma line = [Token(Comma ',', None, line)]
let dot line = [Token(Dot '.', None, line)]
let minus line = [Token(Minus '-', None, line)]
let plus line = [Token(Plus '+', None, line)]
let semicolon line = [Token(Semicolon ';', None, line)]
let star line = [Token(Star '*', None, line)]
let bang_equal line = [Token(Bang_equal "!=", None, line)]
let bang line = [Token(Bang '!', None, line)]
let equal_equal line = [Token(Equal_equal "==", None, line)]
let equal line = [Token(Equal '=', None, line)]
let greater_equal line = [Token(Greater_equal ">=", None, line)]
let greater line = [Token(Greater '>', None, line)]
let less_equal line = [Token(Less_equal "<=", None, line)]

let less line = [Token(Less '<', None, line)]
let slash line = [Token(Less '/', None, line)]
let rec extract_string_from_rest acc chars line = 
  match chars with
  | [] -> (acc, chars, line)
  | '"' :: rest -> (acc, rest, line)
  | '\n' :: rest -> extract_string_from_rest acc rest (line + 1)
  | x :: rest -> extract_string_from_rest (acc @ [x]) rest line
(* let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false *)
let is_digit = function '0' .. '9' -> true | _ -> false
 let rec consume_digits acc chars = match chars with 
          | x :: rest -> if (is_digit x) then consume_digits (acc @ [x]) rest else (acc, rest)
          | [] -> (acc, [])


let rec handle_number acc chars = match chars with
  | [] -> (acc, chars)
  | '.' as dot :: d :: t -> if (d |> is_digit) then consume_digits (acc @ [dot; d]) t else (acc, dot::d::t)
  | h :: t -> if (h |> is_digit) then handle_number (acc @ [h]) t else (acc, h::t)

 let rec scan code line = match code with
  | [] -> eof line
  | '(':: rest -> left_paren line @ scan rest line 
  | ')'  :: rest -> right_paren line @ scan rest line 
  | '{'  :: rest -> left_brace line @ scan rest line
  | '}'  :: rest -> right_brace line @ scan rest line 
  | ','  :: rest -> comma line @ scan rest line 
  | '.'  :: rest -> dot line @ scan rest line 
  | '-'  :: rest -> minus line @ scan rest line 
  | '+'  :: rest -> plus line @ scan rest line 
  | ';'  :: rest -> semicolon line @ scan rest line 
  | '*'  :: rest -> star line @ scan rest line 
  | '!' :: '='  :: rest  -> bang_equal line @ scan rest line 
  | '!' :: rest -> bang line @ scan rest line 
  | '=' :: '=' :: rest  -> equal_equal line @ scan rest line 
  | '=' :: rest -> equal line @ scan rest line 
  | '>' :: '=' :: rest  ->  greater_equal line @ scan rest line 
  | '>' :: rest -> greater line @ scan rest line 
  | '<' :: '=' :: rest  -> less_equal line @ scan rest line 
  | '<' :: rest -> less line @ scan rest line 
  | '/' :: '/' :: rest -> scan (skip_until_newline rest) line
  | '/' :: rest -> slash line @ scan rest line
  | (' ' | '\r' | '\t'):: rest -> scan rest line
  |  '\n' :: rest -> scan rest (line + 1)
  |  '"' :: rest -> 
      let (extracted, r, line) = extract_string_from_rest [] rest line in
      [Token(String_t(chars_to_string(extracted)), None , line)] @ scan r line
  | _other :: rest  -> scan rest line 


let%test _ = let res = scan (string_to_char_list "\"jumanji\"") 0 in
    match res with 
    | [] -> false
    | h :: _ -> (match h with | Token(k, _,_) -> match k with
    | String_t name -> name = "jumanji" | _ -> false)

let%test _ = let (acc, _) = handle_number [] (string_to_char_list "42.123") in
    acc = ['4';'2';'.'; '1'; '2'; '3']