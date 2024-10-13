open Lox_values

type environment = {
  current : (string, lox_primitive) Hashtbl.t;
  enclosing : environment option;
}

let rec define_primitive ({ current; enclosing } : environment) key value =
  enclosing |> function
  | Some p -> define_primitive p key value
  | None -> Hashtbl.replace current key value

let rec get_primitive ({ current; enclosing } : environment) key =
  enclosing |> function
  | Some p -> get_primitive p key
  | None -> Hashtbl.find_opt current key

(* let create_base () : base_t = Hashtbl.create 16 *)
let make_env = { current = Hashtbl.create 16; enclosing = None }

let make_env_with_enclosing enclosing =
  { current = Hashtbl.create 16; enclosing = Some enclosing }

let%test _ =
  let env = make_env_with_enclosing make_env in
  define_primitive env "Test" (Lox_literal (String_literal "Test"));
  get_primitive env "Test" |> function Some _ -> true | None -> false
