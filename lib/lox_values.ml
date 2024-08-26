type _ literal =
  | String_literal : string -> string literal
  | Bool_literal : bool -> bool literal
  | Number_literal : float -> float literal
  | No_literal

type lox_function = Func of string

type lox_primitive =
  | Lox_literal : 'a literal -> lox_primitive
  | Lox_function : lox_function -> lox_primitive
  | No_primitive
(* | Int_literal of string literal *)
(* | No_literal *)