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

let is_at_end code p = p >= String.length code

let rec extract_decimals code p =
  if is_at_end code p then []
  else
    match code.[p] with
    | '0' .. '9' as d -> d :: extract_decimals code (p + 1)
    | '.' ->
        print_endline "Only 1 dot is allowed in decimals!";
        []
    | _ -> []

let char_list_to_float char_list =
  (* Convert the character list to a string *)
  let str = String.of_seq (List.to_seq char_list) in
  (* Convert the string to a float *)
  float_of_string str

let scan_tokens code =
  let rec scan_tokens_aux line pos =
    let is_at_end p = p >= String.length code in
    let next_is ch =
      if is_at_end (pos + 1) then false else code.[pos + 1] = ch
    in
    if is_at_end pos then [ { kind = EOF; line } ]
    else
      match code.[pos] with
      | '(' -> { kind = Left_paren; line } :: scan_tokens_aux line (pos + 1)
      | ')' -> { kind = Right_paren; line } :: scan_tokens_aux line (pos + 1)
      | '{' -> { kind = Left_brace; line } :: scan_tokens_aux line (pos + 1)
      | '}' -> { kind = Right_brace; line } :: scan_tokens_aux line (pos + 1)
      | ',' -> { kind = Comma; line } :: scan_tokens_aux line (pos + 1)
      | '.' -> { kind = Dot; line } :: scan_tokens_aux line (pos + 1)
      | '-' -> { kind = Minus; line } :: scan_tokens_aux line (pos + 1)
      | '+' -> { kind = Plus; line } :: scan_tokens_aux line (pos + 1)
      | ';' -> { kind = Semicolon; line } :: scan_tokens_aux line (pos + 1)
      | '*' -> { kind = Star; line } :: scan_tokens_aux line (pos + 1)
      | '!' ->
          if next_is '=' then
            { kind = Bang_equal; line } :: scan_tokens_aux line (pos + 2)
          else { kind = Bang; line } :: scan_tokens_aux line (pos + 1)
      | '=' ->
          if next_is '=' then
            { kind = Equal_equal; line } :: scan_tokens_aux line (pos + 2)
          else { kind = Equal; line } :: scan_tokens_aux line (pos + 1)
      | '>' ->
          if next_is '=' then
            { kind = Greater_equal; line } :: scan_tokens_aux line (pos + 2)
          else { kind = Greater; line } :: scan_tokens_aux line (pos + 1)
      | '<' ->
          if next_is '=' then
            { kind = Less_equal; line } :: scan_tokens_aux line (pos + 2)
          else { kind = Less; line } :: scan_tokens_aux line (pos + 1)
      | '/' ->
          let rec skip_comment l p =
            if is_at_end p then (l, p)
            else
              match code.[p] with
              | '\n' -> (l + 1, p + 1)
              | _ -> skip_comment l (p + 1)
          in
          if next_is '/' then
            let new_line, new_pos = skip_comment line (pos + 1) in
            scan_tokens_aux new_line new_pos
          else { kind = Slash; line } :: scan_tokens_aux line (pos + 1)
      | ' ' | '\r' | '\t' -> scan_tokens_aux line (pos + 1)
      | '\n' -> scan_tokens_aux (line + 1) (pos + 1)
      | '"' ->
          let rec extract_text p acc =
            match code.[p] with
            | '"' -> (p + 1, acc)
            | x -> extract_text (p + 1) (acc @ [ x ])
          in
          let inc_pos, acc = extract_text (pos + 1) [] in
          { kind = String_t (chars_to_string acc); line }
          :: scan_tokens_aux inc_pos line
      | '0' .. '9' as m ->
          let rec extract_digits p =
            if is_at_end p then []
            else
              match code.[p] with
              | '0' .. '9' as d -> d :: extract_digits (p + 1)
              | '.' -> '.' :: extract_decimals code (p + 1)
              | _ -> []
          in
          let dig = extract_digits (pos + 1) in
          { kind = Number (char_list_to_float (m :: dig)); line = 1 }
          :: scan_tokens_aux line (pos + List.length dig + 1)
      | ('a' .. 'z' | 'A' .. 'Z') as k ->
          (* handle identifer *)
          let rec get_alphanumerics p =
            if is_at_end p then []
            else
              match code.[p] with
              | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as w ->
                  w :: get_alphanumerics (p + 1)
              | _ -> []
          in
          let word = get_alphanumerics (pos + 1) in
          let lookup_word = chars_to_string (k :: word) in
          let keyword = look_up_keyword lookup_word in
          let result_token =
            match keyword with Some x -> x | None -> Identifier lookup_word
          in
          { kind = result_token; line }
          :: scan_tokens_aux line (pos + List.length word + 1)
      | x ->
          print_endline ("Character " ^ String.make 1 x ^ " isn't a known token");
          []
  in
  scan_tokens_aux 0 0

let rec get_tokens_strings = function
  | [] -> ""
  | { kind = tok; _ } :: rest ->
      (tok |> token_to_string) ^ "|" ^ get_tokens_strings rest

let single_token_check tk tokens =
  match tokens with
  | [ { kind = k; _ }; { kind = e; _ } ] -> k = tk && e = EOF
  | toks ->
      let rec print_rest (tokens : token list) =
        match tokens with
        | [] -> ()
        | x :: rest ->
            print_endline (show_token x);
            print_rest rest
      in
      print_endline "----------The Tokens --------";
      print_rest toks;
      print_endline "-----------------------------";
      false

let is_end_of_file tokens =
  match tokens with [ { kind = e; _ } ] -> e = EOF | _ -> false

let%test _ =
  let res = scan_tokens "\"jumanji\"" in
  List.hd res |> function
  | { kind = String_t name; _ } -> name = "jumanji"
  | _ -> false

let%test _ = scan_tokens "(" |> single_token_check Left_paren
let%test _ = scan_tokens ">=" |> single_token_check Greater_equal
let%test _ = scan_tokens "//{}{}{}" |> is_end_of_file
let%test _ = scan_tokens " \t\r==" |> single_token_check Equal_equal
let%test _ = scan_tokens "4.23" |> single_token_check (Number 4.23)
let%test _ = scan_tokens "4.232" |> single_token_check (Number 4.232)

let%test _ =
  let acc, _ = handle_number [] (string_to_char_list "42.123") in
  acc = [ '4'; '2'; '.'; '1'; '2'; '3' ]

let print_tokens tokens = tokens |> get_tokens_strings |> print_endline
