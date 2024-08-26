open Lox_values

module CommonEnv = struct
  type base_t = (string, lox_primitive) Hashtbl.t

  let create_base () : base_t = Hashtbl.create 16

  let define_base (env : base_t) (key : string) (value : lox_primitive) : unit =
    Hashtbl.replace env key value

  let get_base (env : base_t) (key : string) : lox_primitive option =
    Hashtbl.find_opt env key
end

module BaseEnv : sig
  type t

  val create : unit -> t
  val define : t -> string -> lox_primitive -> unit
  val get : t -> string -> lox_primitive option
end = struct
  type t = CommonEnv.base_t

  let create () = CommonEnv.create_base ()
  let define = CommonEnv.define_base
  let get = CommonEnv.get_base
end

module MakeEnv (Enclosing : sig
  type t

  val get : t -> string -> lox_primitive option
end) : sig
  type t

  val create : Enclosing.t option -> t
  val define : t -> string -> lox_primitive -> unit
  val get : t -> string -> lox_primitive option
end = struct
  type t = { base : CommonEnv.base_t; enclosing : Enclosing.t option }

  let create enclosing = { base = CommonEnv.create_base (); enclosing }
  let define env key value = CommonEnv.define_base env.base key value

  let get env key =
    match CommonEnv.get_base env.base key with
    | Some value -> Some value
    | None -> (
        match env.enclosing with
        | Some parent_env -> Enclosing.get parent_env key
        | None -> None)
end
