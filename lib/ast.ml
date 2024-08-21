open Tokens
open Lox_values

type expr =
  | Unary_expr of { op : token; right : expr }
  | Binary_expr of { left : expr; op : token; right : expr }
  | Logical_expr of { left : expr; op : token; right : expr }
  | Group_expr of expr
  | Literal_expr of literal
  | Variable_expr of token
  | Call_expr of { callee : expr; paren : token; args : expr list }
  | Assign_expr of { name : token; value : expr }
(* | Get_expr of { name : token_kind; value : expr } *)
(* | Set_expr of { name : token_kind; value : expr } *)

type stmt =
  | Expression_stmt of expr
  | Var_stmt of { name : token; init : expr option }
  | Block_stmt of stmt list
  | If_stmt of { cond : expr; then_branch : stmt; else_branch : stmt option }
  | While_stmt of { cond : expr; body : stmt }
  | Print_stmt of expr
