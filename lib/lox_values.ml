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

let primitive_as_string = function
  | Lox_literal (String_literal s) -> s
  | Lox_literal (Bool_literal b) -> string_of_bool b
  | Lox_literal (Number_literal f) -> string_of_float f
  | Lox_literal No_literal -> "No literal"
  | Lox_function _ -> "This is a function"
  | No_primitive -> "No Literals"
