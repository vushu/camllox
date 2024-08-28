open Lox_values
open Tokens
open Ast
open Environment

exception RuntimeException of string

module LoxEnv = MakeEnv (BaseEnv)

let environment = LoxEnv.create None

let is_truthy = function
  | No_primitive -> false
  | Lox_literal (Bool_literal x) -> x
  | _ -> true

let check_number_operand = function
  | Lox_literal (Number_literal x) -> x
  | _ -> raise (RuntimeException "Operand must be number.")

let rec evaluate_statement statement =
  match statement with
  | Expression_stmt e -> evaluate_expr e
  | Var_stmt { name; init } -> (
      match (name, init) with
      | { kind = Identifier n; line = _ }, Some e ->
          print_endline ("Define var named: " ^ n);
          LoxEnv.define environment n (evaluate_expr e);
          (LoxEnv.get environment n |> function
           | Some _ -> print_endline "YUP exists"
           | None -> raise (RuntimeException "HUH nothing?"));
          No_primitive
      | _ -> raise (RuntimeException "Failed"))
  | Print_stmt e ->
      print_endline (primitive_as_string (evaluate_expr e));
      No_primitive
  | _ -> No_primitive

and evaluate_expr statement =
  match statement with
  | Unary_expr { op = { kind; _ }; right } -> eval_unary_expr kind right
  | Literal_expr l -> Lox_literal l
  | Group_expr e -> evaluate_expr e
  | Binary_expr { left; op = { kind; _ }; right } ->
      eval_binary_expr left kind right
  | Logical_expr { left; op = { kind; _ }; right } ->
      eval_logical_expr left kind right
  | _ -> No_primitive

(* let add_or_concat (type a) (literal : a literal) (x : a) (y : a) : lox_primitive
     =
   match literal with
   | Number_literal _ -> Lox_literal (Number_literal (x +. y))
   | String_literal _ -> Lox_literal (String_literal (x ^ y))
   | _ -> raise (RuntimeException "Operands must be two numbers or two strings.") *)

and add_or_concat x y =
  match (x, y) with
  | Lox_literal (Number_literal x), Lox_literal (Number_literal y) ->
      Lox_literal (Number_literal (x +. y))
  | Lox_literal (String_literal x), Lox_literal (String_literal y) ->
      Lox_literal (String_literal (x ^ y))
  | _ -> raise (RuntimeException "Operands must be two numbers or two strings.")

and eval_binary_expr (left : expr) (kind : token_kind) (right : expr) =
  let eval_left = evaluate_expr left in
  let eval_right = evaluate_expr right in
  match kind with
  | Minus ->
      Lox_literal
        (Number_literal
           (check_number_operand eval_left -. check_number_operand eval_right))
  | Slash ->
      Lox_literal
        (Number_literal
           (check_number_operand eval_left /. check_number_operand eval_right))
  | Star ->
      Lox_literal
        (Number_literal
           (check_number_operand eval_left *. check_number_operand eval_right))
  | Plus -> add_or_concat eval_left eval_right
  | Greater ->
      Lox_literal
        (Bool_literal
           (check_number_operand eval_left > check_number_operand eval_right))
  | Greater_equal ->
      Lox_literal
        (Bool_literal
           (check_number_operand eval_left >= check_number_operand eval_right))
  | Bang_equal ->
      Lox_literal
        (Bool_literal
           (check_number_operand eval_left <> check_number_operand eval_right))
  | Equal_equal ->
      Lox_literal
        (Bool_literal
           (check_number_operand eval_left == check_number_operand eval_right))
  | Less ->
      Lox_literal
        (Bool_literal
           (check_number_operand eval_left < check_number_operand eval_right))
  | Less_equal ->
      Lox_literal
        (Bool_literal
           (check_number_operand eval_left <= check_number_operand eval_right))
  | op -> raise (RuntimeException ("Unknown operator: " ^ token_to_string op))

and eval_logical_expr left tk right =
  let eval_left = evaluate_expr left in
  match tk with
  | Or -> if is_truthy eval_left then eval_left else evaluate_expr right
  | And -> if not (is_truthy eval_left) then eval_left else evaluate_expr right
  | _ -> Lox_literal (Bool_literal false)

and eval_unary_expr (tk : token_kind) (right : expr) =
  let res = evaluate_expr right in
  match tk with
  | Bang -> Lox_literal (Bool_literal (is_truthy res))
  | Minus -> Lox_literal (Number_literal (-.check_number_operand res))
  | _ -> No_primitive

(* | Variable_expr (_, _) -> No_primitive *)
(* | Binary_expr _ -> No_primitive *)
(* | Logical_expr _ -> print_endline "sss"
   | Group_expr _ -> print_endline "dsfsdf"
   | Literal_expr x -> Literal x
   | Call_expr _ -> print_endline "sdfsdfs"
   | Assign_expr _ -> print_endline "sfsadfas" *)
(* | _ -> No_primitive *)
(* print_endline "Not implemented"; *)

let interpret stmts =
  let rec interpret_aux = function
    | [] -> ()
    | x :: xs ->
        let _prim = evaluate_statement x in
        interpret_aux xs
  in
  let _res = interpret_aux stmts in
  print_endline "sdfdsf"

let%test _ =
  let _res =
    evaluate_expr
      (Call_expr
         {
           callee = Literal_expr (Bool_literal true);
           paren = { kind = Right_paren; line = 1 };
           args = [];
         })
  in
  true

let%test _ =
  let _res = evaluate_expr (Variable_expr { kind = Right_paren; line = 1 }) in
  true

let%test _ =
  let _res =
    evaluate_expr
      (Group_expr
         (Assign_expr
            {
              name = { kind = Right_paren; line = 1 };
              value = Literal_expr (Bool_literal true);
            }))
  in
  true

let%test _ =
  let _res = evaluate_expr (Variable_expr { kind = Right_paren; line = 1 }) in
  true

let%test _ =
  let _res = evaluate_expr (Literal_expr (Number_literal 1.)) in
  true

let%test _ =
  let res =
    evaluate_expr
      (Unary_expr
         {
           op = { kind = Minus; line = 1 };
           right = Literal_expr (Number_literal 1.);
         })
  in
  res |> function Lox_literal (Number_literal x) -> x = -1. | _ -> false

let%test _ =
  let expr =
    Binary_expr
      {
        left = Literal_expr (Number_literal 1.);
        op = { kind = Plus; line = 0 };
        right = Literal_expr (Number_literal 2.);
      }
  in
  evaluate_expr expr |> function
  | Lox_literal (Number_literal x) -> x = 3.
  | _ -> false

let%test _ =
  let expr =
    Binary_expr
      {
        left = Literal_expr (String_literal "ab");
        op = { kind = Plus; line = 0 };
        right = Literal_expr (String_literal "cd");
      }
  in
  evaluate_expr expr |> function
  | Lox_literal (String_literal x) -> x = "abcd"
  | _ -> false

let%test _ =
  let expr =
    Binary_expr
      {
        left = Literal_expr (Number_literal 3.);
        op = { kind = Star; line = 0 };
        right = Literal_expr (Number_literal 3.);
      }
  in
  evaluate_expr expr |> function
  | Lox_literal (Number_literal x) -> x = 9.
  | _ -> false

let%test _ =
  let expr =
    Logical_expr
      {
        left = Literal_expr (Bool_literal false);
        op = { kind = Or; line = 0 };
        right = Literal_expr (Bool_literal true);
      }
  in
  evaluate_expr expr |> function
  | Lox_literal (Bool_literal x) -> x = true
  | _ -> false

let%test _ =
  let e = Literal_expr (Bool_literal false) in
  let stmt =
    Var_stmt { name = { kind = Identifier "Hello"; line = 0 }; init = Some e }
  in
  evaluate_statement stmt |> function No_primitive -> true | _ -> false

let%test _ =
  let e = Literal_expr (String_literal "This is a print line") in
  let stmt = Print_stmt e in
  evaluate_statement stmt |> function No_primitive -> true | _ -> false

(* in
   evaluate expr |> function
   | Lox_literal (Bool_literal x) -> x = true
   | _ -> false
*)
(* type token = {kind : token_kind; line : int} *)
