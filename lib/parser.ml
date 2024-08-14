open Tokens
open Ast

let primary = function
  | [] -> (Literal_expr (String_t "FAILED"), [])
  | (((False | True | Nil) as t), _) :: rest -> (Literal_expr t, rest)
  | (((String_t _ | Identifier _ | Number _) as t), _) :: rest ->
      (Literal_expr t, rest)
  (* Grouping *)
  | ((Left_paren as t), _) :: rest -> (Literal_expr t, rest)
  | _ :: rest ->
      print_endline "Failed to parse";
      (Literal_expr (String_t "Failed to parse"), rest)

let rec unary = function
  | (((Bang | Minus) as t), _) :: rest ->
      let right, r = unary rest in
      (Unary_expr { op = t; right }, r)
  | x -> primary x

let factor tokens =
  let left, r = unary tokens in
  match r with
  | (((Slash | Star) as t), _) :: rest ->
      let right, r = unary rest in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

let term tokens =
  let left, r = factor tokens in
  match r with
  | (((Minus | Plus) as t), _) :: r ->
      let right, r = factor r in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

let comparison tokens =
  let left, r = term tokens in
  match r with
  | (((Greater | Greater_equal | Less | Less_equal) as t), _) :: r ->
      let right, r = factor r in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

let equality tokens =
  let left, r = comparison tokens in
  match r with
  | (((Bang_equal | Equal_equal) as t), _) :: r ->
      let right, r = comparison r in
      (Binary_expr { op = t; left; right }, r)
  | _ -> (left, r)

let and_expression tokens =
  let left, r = equality tokens in
  match r with
  | ((And as t), _) :: r ->
      let right, r = equality r in
      (Logical_expr { op = t; left; right }, r)
  | _ -> (left, r)

let or_expression tokens =
  let left, r = and_expression tokens in
  match r with
  | ((Or as t), _) :: r ->
      let right, r = and_expression r in
      (Logical_expr { op = t; left; right }, r)
  | _ -> (left, r)

let parse tokens =
  let res, _ = or_expression tokens in
  res

let%test _ =
  let exprs = parse [ (Minus, 1); (Number 1., 1) ] in
  match exprs with Unary_expr { op; _ } -> op = Minus | _ -> false

let%test _ =
  let exprs = parse [ (Number 1., 1); (Plus, 1); (Number 1., 1) ] in
  match exprs with Binary_expr { op; _ } -> op = Plus | _ -> false

let%test _ =
  let exprs = parse [ (Number 1., 1); (Greater, 1); (Number 1., 1) ] in
  match exprs with Binary_expr { op; _ } -> op = Greater | _ -> false
