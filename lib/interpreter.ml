open Tokens
open Ast
open Lox_values

exception RuntimeException of string

let is_truthy = function
  | No_primitive -> false
  | Lox_literal (Bool_literal x) -> x
  | _ -> true

let check_number_operand = function
  | Lox_literal (Number_literal _ as l) -> l
  | _ -> raise (RuntimeException "Operand must be number not: ")

let rec evaluate statement =
  match statement with
  | Unary_expr { op = { kind; _ }; right } -> eval_unary_expr kind right
  | _ -> No_primitive

and eval_unary_expr tk right =
  let primitive = evaluate right in
  match tk with
  | Bang -> Lox_literal (Bool_literal (is_truthy primitive))
  | Minus -> (
      check_number_operand primitive |> function
      | Number_literal x -> Lox_literal (Number_literal (x *. -1.))
      | No_literal -> No_primitive)
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
    | _ :: xs ->
        (* let _prim = evaluate x in *)
        interpret_aux xs
  in
  let _res = interpret_aux stmts in
  print_endline "sdfdsf"

let%test _ =
  let _res =
    evaluate
      (Call_expr
         {
           callee = Literal_expr (Bool_literal true);
           paren = { kind = Right_paren; line = 1 };
           args = [];
         })
  in
  true

let%test _ =
  let _res = evaluate (Variable_expr { kind = Right_paren; line = 1 }) in
  true

let%test _ =
  let _res =
    evaluate
      (Group_expr
         (Assign_expr
            {
              name = { kind = Right_paren; line = 1 };
              value = Literal_expr (Bool_literal true);
            }))
  in
  true

let%test _ =
  let _res = evaluate (Variable_expr { kind = Right_paren; line = 1 }) in
  true

let%test _ =
  let _res = evaluate (Literal_expr (Number_literal 1.)) in
  true

let%test _ =
  let _res =
    evaluate
      (Unary_expr
         {
           op = { kind = Plus; line = 1 };
           right = Literal_expr (Number_literal 1.);
         })
  in
  true
