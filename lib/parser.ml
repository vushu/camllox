open Tokens
open Ast

exception ParseException of string

let consume tk message = function
  | [] ->
      print_endline "List is empty.";
      raise (ParseException message)
  | (kind, _) :: xs -> if kind = tk then xs else raise (ParseException message)

let rec primary = function
  | [] -> (Literal_expr (String_t "FAILED"), [])
  | (((False | True | Nil) as t), _) :: rest -> (Literal_expr t, rest)
  | (((String_t _ | Number _) as t), _) :: rest -> (Literal_expr t, rest)
  | ((Identifier _ as t), _) :: rest -> (Variable_expr t, rest)
  (* Grouping *)
  | (Left_paren, _) :: rest -> (
      let e, r = expression rest in
      match r with
      | (Right_paren, _) :: r ->
          (* print_endline "Created Group expression succesfully"; *)
          (Group_expr e, r)
      | _ -> raise (ParseException "Failed to parse group expression"))
  | _ -> raise (ParseException "Unknown parse case")

and finish_call callee tokens =
  match tokens with
  | ((Right_paren, _) as t) :: rest -> (callee, t :: rest)
  | arg ->
      let rec consume_args args toks =
        let e, r = expression toks in
        r |> function
        | (Comma, _) :: r -> consume_args (args @ [ e ]) r
        | ((Right_paren as t), _) :: r ->
            (Call_expr { callee; paren = t; args }, r)
        | _ -> raise (ParseException "Expected ')' after arguments")
      in
      consume_args [] arg

and call tokens =
  let e, r = primary tokens in
  match r with (Left_paren, _) :: r -> finish_call e r | all -> (e, all)

and unary = function
  | (((Bang | Minus) as t), _) :: rest ->
      let right, r = unary rest in
      (Unary_expr { op = t; right }, r)
  | x -> call x

and factor tokens =
  let left, r = unary tokens in
  match r with
  | (((Slash | Star) as t), _) :: rest ->
      let right, r = unary rest in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

and term tokens =
  let left, r = factor tokens in
  match r with
  | (((Minus | Plus) as t), _) :: r ->
      let right, r = factor r in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

and comparison tokens =
  let left, r = term tokens in
  match r with
  | (((Greater | Greater_equal | Less | Less_equal) as t), _) :: r ->
      let right, r = factor r in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

and equality tokens =
  let left, r = comparison tokens in
  match r with
  | (((Bang_equal | Equal_equal) as t), _) :: r ->
      let right, r = comparison r in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

and and_expression tokens =
  let left, r = equality tokens in
  match r with
  | ((And as t), _) :: r ->
      let right, r = equality r in
      (Logical_expr { op = t; left; right }, r)
  | _ -> (left, r)

and or_expression tokens =
  let left, r = and_expression tokens in
  match r with
  | ((Or as t), _) :: r ->
      let right, r = and_expression r in
      (Logical_expr { op = t; left; right }, r)
  | _ -> (left, r)

and assigment tokens =
  let e, r = or_expression tokens in
  match r with
  | ((Equal as tk), _) :: r -> (
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
  | ((Identifier _ as name), _) :: r -> (
      r |> function
      | (Equal, _) :: r ->
          let e, r = expression r in
          let r =
            consume Semicolon "Expected ';' after variable declaration." r
          in
          (Var_stmt { name; init = Some e }, r)
      | x -> (Var_stmt { name; init = None }, x))
  | _ -> raise (ParseException "Expect variable name.")

and declaration = function
  | (Var, 1) :: rest -> var_declaration rest
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
    | (Right_brace, _) :: rest -> (stmts, rest)
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
  | (Else, _) :: r ->
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
    | (Semicolon, _) :: r -> (None, r)
    | (Var, __) :: r ->
        let e, r = var_declaration r in
        (Some e, r)
    | x ->
        let e, r = expression_statement x in
        (Some e, r)
  in
  let cond, r =
    match r with
    | ((Semicolon, _) as t) :: r -> (None, t :: r)
    | x ->
        let e, r = expression x in
        (Some e, r)
  in
  let r = consume Semicolon "Expected ';' after loop condition." r in
  let increment, r =
    match r with
    | [] -> raise (ParseException "Expected ')' after for clauses.")
    | (Right_paren, _) :: r -> (None, r)
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
    if Option.is_none cond then While_stmt { cond = Literal_expr True; body }
    else While_stmt { cond = Option.get cond; body }
  in

  let body =
    match init with None -> body | Some init -> Block_stmt [ body; init ]
  in
  (body, r)

and statement = function
  | (Print, _) :: r -> print_statement r
  | (Left_brace, _) :: r ->
      let stmts, r = block r in
      (Block_stmt stmts, r)
  | (If, _) :: r -> if_statement r
  | (While, _) :: r -> while_statement r
  | (For, _) :: r -> for_statement r
  | x -> expression_statement x

let parse tokens =
  let rec get_statements stmts toks =
    match toks with
    | [] -> stmts
    | [ (EOF, _) ] -> stmts
    | xs ->
        let s, r = declaration xs in
        get_statements (stmts @ [ s ]) r
  in
  get_statements [] tokens

let%test _ =
  let exprs, _ = unary [ (Minus, 1); (Number 1., 1) ] in
  match exprs with Unary_expr { op; _ } -> op = Minus | _ -> false

let%test _ =
  let exprs, _ = term [ (Number 1., 1); (Plus, 1); (Number 1., 1) ] in
  match exprs with Binary_expr { op; _ } -> op = Plus | _ -> false

let%test _ =
  let exprs, _ = and_expression [ (True, 1); (And, 1); (True, 1) ] in
  match exprs with Logical_expr { op; _ } -> op = And | _ -> false

let%test _ =
  try
    let exprs, _ = primary [ (Left_paren, 1); (True, 1); (Right_paren, 1) ] in
    match exprs with Group_expr _ -> true | _ -> false
  with ParseException msg ->
    print_endline msg;
    false

let%test _ =
  let stmts = var_declaration [ (Identifier "Mama", 1) ] in
  match stmts with
  | Var_stmt { name; init }, _ ->
      name = Identifier "Mama" && Option.is_none init
  | _ -> false

let%test _ =
  let stmts =
    var_declaration
      [ (Identifier "Mama", 1); (Equal, 1); (Number 1., 1); (Semicolon, 1) ]
  in
  match stmts with
  | Var_stmt { name; init }, _ ->
      name = Identifier "Mama" && Option.is_some init
  | _ -> false

let%test _ =
  let stmts =
    parse [ (Identifier "Mama", 1); (Equal, 1); (Number 1., 1); (Semicolon, 1) ]
    |> List.hd
  in
  match stmts with Expression_stmt (Assign_expr _) -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        (If, 1);
        (Left_paren, 1);
        (True, 1);
        (Right_paren, 1);
        (Left_brace, 1);
        (Identifier "hej", 1);
        (Semicolon, 1);
        (Right_brace, 1);
        (Else, 1);
        (Left_brace, 1);
        (Right_brace, 1);
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
        (If, 1);
        (Left_paren, 1);
        (True, 1);
        (Right_paren, 1);
        (Left_brace, 1);
        (Identifier "hej", 1);
        (Semicolon, 1);
        (Right_brace, 1);
      ]
    |> List.hd
  in
  match stmts with If_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        (If, 1);
        (Left_paren, 1);
        (True, 1);
        (Right_paren, 1);
        (Left_brace, 1);
        (Identifier "hej", 1);
        (Semicolon, 1);
        (Right_brace, 1);
      ]
    |> List.hd
  in
  match stmts with If_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        (Left_brace, 1); (Identifier "hej", 1); (Semicolon, 1); (Right_brace, 1);
      ]
    |> List.hd
  in
  match stmts with Block_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse [ (Print, 1); (Identifier "hej", 1); (Semicolon, 1) ] |> List.hd
  in
  match stmts with Print_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        (While, 1);
        (Left_paren, 1);
        (True, 1);
        (Right_paren, 1);
        (Print, 1);
        (Identifier "hej", 1);
        (Semicolon, 1);
      ]
    |> List.hd
  in
  match stmts with While_stmt _ -> true | _ -> false

let%test _ =
  let stmts =
    parse
      [
        (For, 1);
        (Left_paren, 1);
        (Semicolon, 1);
        (Semicolon, 1);
        (Right_paren, 1);
        (Left_brace, 1);
        (Right_brace, 1);
      ]
    |> List.hd
  in
  match stmts with While_stmt _ -> true | _ -> false
