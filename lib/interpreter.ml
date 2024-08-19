(* open Tokens *)
(* open Ast *)

(* let evaluate (Unary_expr { op = _; right = _ }) = print_endline "" *)
(* let evaluate (Variable_expr _tk) = print_endline "" *)
(* let evaluate (Assign_expr { name = _; value = _ }) = print_endline "" *)
(* let evaluate (Group_expr _expr) = print_endline "" *)

let interpret stmts =
  let rec interpret_aux = function
    | [] -> []
    | _ :: xs ->
        (* evaluate x; *)
        interpret_aux xs
  in
  interpret_aux stmts

let%test _ =
  (* let _res = evaluate (Unary_expr { op = Plus; right = Literal_expr (Number 1.) }) in *)
  true
