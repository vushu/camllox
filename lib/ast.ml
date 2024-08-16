open Tokens

type expr =
  | Unary_expr of { op : token_kind; right : expr }
  | Binary_expr of { left : expr; op : token_kind; right : expr }
  | Logical_expr of { left : expr; op : token_kind; right : expr }
  | Group_expr of expr
  | Literal_expr of token_kind
  | Variable_expr of token_kind
  | Call_expr of { callee : expr; paren : token_kind; args : expr list }

type stmt =
  | Expression_stmt of expr
  | Var_stmt of { name : token_kind; init : expr }
  | Block_stmt of stmt list
  | If_stmt of { cond : expr; then_branch : stmt; else_branch : stmt }
  | While_Stmt of { cond : expr; body : stmt }
