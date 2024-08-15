open Tokens
open Ast

let rec primary = function
  | [] -> (Literal_expr (String_t "FAILED"), [])
  | (((False | True | Nil) as t), _) :: rest -> (Literal_expr t, rest)
  | (((String_t _ | Identifier _ | Number _) as t), _) :: rest ->
      (Literal_expr t, rest)
  (* Grouping *)
  | (Left_paren, _) :: rest -> (
      let e, r = expression rest in
      match r with
      | (Right_paren, _) :: r ->
          print_endline "Created Group expression succesfully";
          (Group_expr e, r)
      | _ ->
          print_endline "Failed to parse group expression";
          (e, r))
  | _ ->
      print_endline "Failed to parse";
      (Group_expr (Literal_expr (String_t "Failed to parse")), [])

and unary = function
  | (((Bang | Minus) as t), _) :: rest ->
      let right, r = unary rest in
      (Unary_expr { op = t; right }, r)
  | x -> primary x

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

and expression tokens = or_expression tokens

let parse tokens =
  let res, _ = expression tokens in
  res

let%test _ =
  let exprs = parse [ (Minus, 1); (Number 1., 1) ] in
  match exprs with Unary_expr { op; _ } -> op = Minus | _ -> false

let%test _ =
  let exprs = parse [ (Number 1., 1); (Plus, 1); (Number 1., 1) ] in
  match exprs with Binary_expr { op; _ } -> op = Plus | _ -> false

let%test _ =
  let exprs = parse [ (True, 1); (And, 1); (True, 1) ] in
  match exprs with Logical_expr { op; _ } -> op = And | _ -> false

let%test _ =
  let exprs = parse [ (Left_paren, 1); (True, 1); (Right_paren, 1) ] in
  match exprs with Group_expr _ -> true | _ -> false
