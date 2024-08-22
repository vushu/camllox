open Tokens
open Ast
open Lox_values

exception ParseException of string

let consume tk message = function
  | [] ->
      print_endline "List is empty.";
      raise (ParseException message)
  | { kind; _ } :: xs ->
      if kind = tk then xs else raise (ParseException message)

let rec primary = function
  | [] -> (Literal_expr (String_literal "FAILED"), [])
  | { kind = False; line = _ } :: rest ->
      (Literal_expr (Bool_literal false), rest)
  | { kind = True; line = _ } :: rest -> (Literal_expr (Bool_literal true), rest)
  | { kind = Nil; line = _ } :: rest -> (Literal_expr No_literal, rest)
  | { kind = String_t x; line = _ } :: rest ->
      (Literal_expr (String_literal x), rest)
  | { kind = Number x; _ } :: rest -> (Literal_expr (Number_literal x), rest)
  | ({ kind = Identifier _; line = _ } as t) :: rest -> (Variable_expr t, rest)
  (* Grouping *)
  | { kind = Left_paren; line = _ } :: rest -> (
      let e, r = expression rest in
      match r with
      | { kind = Right_paren; line = _ } :: r ->
          (* print_endline "Created Group expression succesfully"; *)
          (Group_expr e, r)
      | _ -> raise (ParseException "Failed to parse group expression"))
  | _ -> raise (ParseException "Unknown parse case")

and finish_call callee tokens =
  match tokens with
  | ({ kind = Right_paren; line = _ } as t) :: rest -> (callee, t :: rest)
  | arg ->
      let rec consume_args args toks =
        let e, r = expression toks in
        r |> function
        | { kind = Comma; line = _ } :: r -> consume_args (args @ [ e ]) r
        | ({ kind = Right_paren; _ } as t) :: r ->
            (Call_expr { callee; paren = t; args }, r)
        | _ -> raise (ParseException "Expected ')' after arguments")
      in
      consume_args [] arg

and call tokens =
  let e, r = primary tokens in
  match r with
  | { kind = Left_paren; _ } :: r -> finish_call e r
  | all -> (e, all)

and unary = function
  | ({ kind = Bang | Minus; _ } as t) :: rest ->
      let right, r = unary rest in
      (Unary_expr { op = t; right }, r)
  | x -> call x

and factor tokens =
  let left, r = unary tokens in
  match r with
  | ({ kind = Slash | Star; _ } as t) :: rest ->
      let right, r = unary rest in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

and term tokens =
  let left, r = factor tokens in
  match r with
  | ({ kind = Minus | Plus; _ } as t) :: r ->
      let right, r = factor r in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

and comparison tokens =
  let left, r = term tokens in
  match r with
  | ({ kind = Greater | Greater_equal | Less | Less_equal; _ } as t) :: r ->
      let right, r = factor r in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

and equality tokens =
  let left, r = comparison tokens in
  match r with
  | ({ kind = Bang_equal | Equal_equal; _ } as t) :: r ->
      let right, r = comparison r in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

and and_expression tokens =
  let left, r = equality tokens in
  match r with
  | ({ kind = And; _ } as t) :: r ->
      let right, r = equality r in
      (Logical_expr { op = t; left; right }, r)
  | _ -> (left, r)

and or_expression tokens =
  let left, r = and_expression tokens in
  match r with
  | ({ kind = Or; _ } as t) :: r ->
      let right, r = and_expression r in
      (Logical_expr { op = t; left; right }, r)
  | _ -> (left, r)

and assigment tokens =
  let e, r = or_expression tokens in
  match r with
  | { kind = Equal as tk; _ } :: r -> (
      let value, r = assigment r in
      match e with
      | Variable_expr n -> (Assign_expr { name = n; value }, r)
      | _ ->
          raise
            (ParseException (token_to_string tk ^ " Invalid assignment target."))
      )
  | _ -> (e, r)

and expression tokens = assigment tokens

and var_declaration tokens =
  match tokens with
  | ({ kind = Identifier _; _ } as name) :: r -> (
      r |> function
      | { kind = Equal; _ } :: r ->
          let e, r = expression r in
          let r =
            consume Semicolon "Expected ';' after variable declaration." r
          in
          (Var_stmt { name; init = Some e }, r)
      | x -> (Var_stmt { name; init = None }, x))
  | _ -> raise (ParseException "Expect variable name.")

and declaration = function
  | { kind = Var; line = 1 } :: rest -> var_declaration rest
  | x -> statement x

and expression_statement tokens =
  let e, r = expression tokens in
  let r = consume Semicolon "Expected ';' after variable declaration." r in
  (Expression_stmt e, r)

and print_statement tokens =
  let e, r = expression tokens in
  let r = consume Semicolon "Expected ';' after value." r in
  (Print_stmt e, r)

and block tokens =
  let rec get_statements stmts toks =
    match toks with
    | [] -> raise (ParseException "Expected '}' after block.")
    | { kind = Right_brace; _ } :: rest -> (stmts, rest)
    | x ->
        let s, r = declaration x in
        get_statements (stmts @ [ s ]) r
  in
  get_statements [] tokens

and if_statement tokens =
  let r = consume Left_paren "Expected '(' after 'if' ." tokens in
  let cond, r = expression r in
  let r = consume Right_paren "Expected ')' after if condition ." r in
  (* Then branch *)
  let then_branch, r = statement r in
  match r with
  | { kind = Else; _ } :: r ->
      let else_branch, r = statement r in
      (If_stmt { cond; then_branch; else_branch = Some else_branch }, r)
  | x -> (If_stmt { cond; then_branch; else_branch = None }, x)

and while_statement tokens =
  let cond, r =
    consume Left_paren "Expected '(' after 'while'." tokens |> expression
  in

  let r = consume Right_paren "Expected ')' after 'condition'." r in
  let body, r = statement r in
  (While_stmt { cond; body }, r)

and for_statement tokens =
  let init, r =
    consume Left_paren "Expected '(' after 'for'." tokens |> function
    | { kind = Semicolon; _ } :: r -> (None, r)
    | { kind = Var; _ } :: r ->
        let e, r = var_declaration r in
        (Some e, r)
    | x ->
        let e, r = expression_statement x in
        (Some e, r)
  in
  let cond, r =
    match r with
    | ({ kind = Semicolon; _ } as t) :: r -> (None, t :: r)
    | x ->
        let e, r = expression x in
        (Some e, r)
  in
  let r = consume Semicolon "Expected ';' after loop condition." r in
  let increment, r =
    match r with
    | [] -> raise (ParseException "Expected ')' after for clauses.")
    | { kind = Right_paren; _ } :: r -> (None, r)
    | x ->
        let e, r = expression x in
        (Some e, r)
  in

  let body, r = statement r in
  let body =
    if Option.is_none increment then body
    else
      let expr_stmts = Expression_stmt (Option.get increment) in
      Block_stmt [ expr_stmts; body ]
  in

  let body =
    if Option.is_none cond then
      While_stmt { cond = Literal_expr (Bool_literal true); body }
    else While_stmt { cond = Option.get cond; body }
  in

  let body =
    match init with None -> body | Some init -> Block_stmt [ body; init ]
  in
  (body, r)

and statement = function
  | { kind = Print; _ } :: r -> print_statement r
  | { kind = Left_brace; _ } :: r ->
      let stmts, r = block r in
      (Block_stmt stmts, r)
  | { kind = If; _ } :: r -> if_statement r
  | { kind = While; _ } :: r -> while_statement r
  | { kind = For; _ } :: r -> for_statement r
  | x -> expression_statement x

let parse tokens =
  let rec get_statements stmts toks =
    match toks with
    | [] -> stmts
    | [ { kind = EOF; _ } ] -> stmts
    | xs ->
        let s, r = declaration xs in
        get_statements (stmts @ [ s ]) r
  in
  get_statements [] tokens

let%test _ =
  let exprs, _ =
    unary [ { kind = Minus; line = 1 }; { kind = Number 1.; line = 1 } ]
  in
  match exprs with
  | Unary_expr { op; _ } -> op = { kind = Minus; line = 1 }
  | _ -> false

let%test _ =
  let exprs, _ =
    term
      [
        { kind = Number 1.; line = 1 };
        { kind = Plus; line = 1 };
        { kind = Number 1.; line = 1 };
      ]
  in
  match exprs with
  | Binary_expr { op; _ } -> op = { kind = Plus; line = 1 }
  | _ -> false

let%test _ =
  let exprs, _ =
    and_expression
      [
        { kind = True; line = 1 };
        { kind = And; line = 1 };
        { kind = True; line = 1 };
      ]
  in
  match exprs with
  | Logical_expr { op; _ } -> op = { kind = And; line = 1 }
  | _ -> false

let%test _ =
  try
    let exprs, _ =
      primary
        [
          { kind = Left_paren; line = 1 };
          { kind = True; line = 1 };
          { kind = Right_paren; line = 1 };
        ]
    in
    match exprs with Group_expr _ -> true | _ -> false
  with ParseException msg ->
    print_endline msg;
    false

let%test _ =
  let stmts = var_declaration [ { kind = Identifier "Mama"; line = 1 } ] in
  match stmts with
  | Var_stmt { name; init }, _ ->
      name = { kind = Identifier "Mama"; line = 1 } && Option.is_none init
  | _ -> false

let%test _ =
  let stmts =
    var_declaration
      [
        { kind = Identifier "Mama"; line = 1 };
        { kind = Equal; line = 1 };
        { kind = Number 1.; line = 1 };
        { kind = Semicolon; line = 1 };
      ]
  in
  match stmts with
  | Var_stmt { name; init }, _ ->
      name = { kind = Identifier "Mama"; line = 1 } && Option.is_some init
  | _ -> false

let%test _ =
  let stmts =
    parse
      [
        { kind = Identifier "Mama"; line = 1 };
        { kind = Equal; line = 1 };
        { kind = Number 1.; line = 1 };
        { kind = Semicolon; line = 1 };
      ]
    |> List.hd
  in
  match stmts with Expression_stmt (Assign_expr _) -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        { kind = If; line = 1 };
        { kind = Left_paren; line = 1 };
        { kind = True; line = 1 };
        { kind = Right_paren; line = 1 };
        { kind = Left_brace; line = 1 };
        { kind = Identifier "hej"; line = 1 };
        { kind = Semicolon; line = 1 };
        { kind = Right_brace; line = 1 };
        { kind = Else; line = 1 };
        { kind = Left_brace; line = 1 };
        { kind = Right_brace; line = 1 };
      ]
    |> List.hd
  in
  match stmts with
  | If_stmt { cond = _; then_branch = _; else_branch } ->
      Option.is_some else_branch
  | _ -> false

let%test _ =
  let stmts =
    parse
      [
        { kind = If; line = 1 };
        { kind = Left_paren; line = 1 };
        { kind = True; line = 1 };
        { kind = Right_paren; line = 1 };
        { kind = Left_brace; line = 1 };
        { kind = Identifier "hej"; line = 1 };
        { kind = Semicolon; line = 1 };
        { kind = Right_brace; line = 1 };
      ]
    |> List.hd
  in
  match stmts with If_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        { kind = If; line = 1 };
        { kind = Left_paren; line = 1 };
        { kind = True; line = 1 };
        { kind = Right_paren; line = 1 };
        { kind = Left_brace; line = 1 };
        { kind = Identifier "hej"; line = 1 };
        { kind = Semicolon; line = 1 };
        { kind = Right_brace; line = 1 };
      ]
    |> List.hd
  in
  match stmts with If_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        { kind = Left_brace; line = 1 };
        { kind = Identifier "hej"; line = 1 };
        { kind = Semicolon; line = 1 };
        { kind = Right_brace; line = 1 };
      ]
    |> List.hd
  in
  match stmts with Block_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        { kind = Print; line = 1 };
        { kind = Identifier "hej"; line = 1 };
        { kind = Semicolon; line = 1 };
      ]
    |> List.hd
  in
  match stmts with Print_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        { kind = While; line = 1 };
        { kind = Left_paren; line = 1 };
        { kind = True; line = 1 };
        { kind = Right_paren; line = 1 };
        { kind = Print; line = 1 };
        { kind = Identifier "hej"; line = 1 };
        { kind = Semicolon; line = 1 };
      ]
    |> List.hd
  in
  match stmts with While_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        { kind = For; line = 1 };
        { kind = Left_paren; line = 1 };
        { kind = Semicolon; line = 1 };
        { kind = Semicolon; line = 1 };
        { kind = Right_paren; line = 1 };
        { kind = Left_brace; line = 1 };
        { kind = Right_brace; line = 1 };
      ]
    |> List.hd
  in
  match stmts with While_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        { kind = For; line = 1 };
        { kind = Left_paren; line = 1 };
        { kind = Var; line = 1 };
        { kind = Identifier "my_var"; line = 1 };
        { kind = Equal; line = 1 };
        { kind = Number 0.; line = 1 };
        { kind = Semicolon; line = 1 };
        { kind = Semicolon; line = 1 };
        { kind = Right_paren; line = 1 };
        { kind = Left_brace; line = 1 };
        { kind = Right_brace; line = 1 };
      ]
    |> List.hd
  in
  match stmts with Block_stmt _ -> true | _ -> false
