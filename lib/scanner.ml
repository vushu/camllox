open Tokens

(* let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false *)
let is_digit = function '0' .. '9' -> true | _ -> false

let rec consume_digits acc chars =
  match chars with
  | x :: rest ->
      if is_digit x then consume_digits (acc @ [ x ]) rest else (acc, rest)
  | [] -> (acc, [])

let rec handle_number acc chars =
  match chars with
  | [] -> (acc, chars)
  | ('.' as dot) :: d :: t ->
      if d |> is_digit then consume_digits (acc @ [ dot; d ]) t
      else (acc, dot :: d :: t)
  | h :: t ->
      if h |> is_digit then handle_number (acc @ [ h ]) t else (acc, h :: t)

let scan_tokens code =
  let rec scan_tokens_aux line pos =
    let is_at_end p = p >= String.length code in
    let next_is ch =
      if is_at_end (pos + 1) then false else code.[pos + 1] = ch
    in
    if is_at_end pos then [ (EOF, line) ]
    else
      match code.[pos] with
      | '(' -> (Left_paren, line) :: scan_tokens_aux line (pos + 1)
      | ')' -> (Right_paren, line) :: scan_tokens_aux line (pos + 1)
      | '{' -> (Left_brace, line) :: scan_tokens_aux line (pos + 1)
      | '}' -> (Right_brace, line) :: scan_tokens_aux line (pos + 1)
      | ',' -> (Comma, line) :: scan_tokens_aux line (pos + 1)
      | '.' -> (Dot, line) :: scan_tokens_aux line (pos + 1)
      | '-' -> (Minus, line) :: scan_tokens_aux line (pos + 1)
      | '+' -> (Plus, line) :: scan_tokens_aux line (pos + 1)
      | ';' -> (Semicolon, line) :: scan_tokens_aux line (pos + 1)
      | '*' -> (Star, line) :: scan_tokens_aux line (pos + 1)
      | '!' ->
          if next_is '=' then
            (Bang_equal, line) :: scan_tokens_aux line (pos + 2)
          else (Bang, line) :: scan_tokens_aux line (pos + 1)
      | '=' ->
          if next_is '=' then
            (Equal_equal, line) :: scan_tokens_aux line (pos + 2)
          else (Equal, line) :: scan_tokens_aux line (pos + 1)
      | '>' ->
          if next_is '=' then
            (Greater_equal, line) :: scan_tokens_aux line (pos + 2)
          else (Greater, line) :: scan_tokens_aux line (pos + 1)
      | '<' ->
          if next_is '=' then
            (Less_equal, line) :: scan_tokens_aux line (pos + 2)
          else (Less, line) :: scan_tokens_aux line (pos + 1)
      | '/' ->
          if next_is '/' then scan_tokens_aux line (pos + 2)
          else (Slash, line) :: scan_tokens_aux line (pos + 1)
      | ' ' | '\r' | '\t' -> scan_tokens_aux line (pos + 1)
      | '\n' -> scan_tokens_aux (line + 1) (pos + 1)
      | '"' ->
          let rec extract_text p acc =
            match code.[p] with
            | '"' -> (p + 1, acc)
            | x -> extract_text (p + 1) (acc @ [ x ])
          in
          let inc_pos, acc = extract_text (pos + 1) [] in
          (String_t (chars_to_string acc), line) :: scan_tokens_aux inc_pos line
      | _ -> []
  in
  scan_tokens_aux 0 0

let rec get_tokens_strings = function
  | [] -> ""
  | (tok, _) :: rest -> (tok |> token_to_string) ^ "|" ^ get_tokens_strings rest

let single_check tk tokens =
  match tokens with [ (k, _); (e, _) ] -> k = tk && e = EOF | _ -> false

let%test _ =
  let res = scan_tokens "\"jumanji\"" in
  List.hd res |> function String_t name, _ -> name = "jumanji" | _ -> false

let%test _ = scan_tokens "(" |> single_check Left_paren
let%test _ = scan_tokens ">=" |> single_check Greater_equal

let%test _ =
  let acc, _ = handle_number [] (string_to_char_list "42.123") in
  acc = [ '4'; '2'; '.'; '1'; '2'; '3' ]

let print_tokens tokens = tokens |> get_tokens_strings |> print_string
(* let%test _ =
   scan_tokens "(" |> get_tokens_strings |> print_string;
   true *)
