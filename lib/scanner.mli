open Tokens

val single_check : token_kind -> token list -> bool
val scan_tokens : string -> token list
val print_tokens : token list -> unit
