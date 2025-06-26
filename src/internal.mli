exception Read_error of string
exception Write_error of string

module type T =
    sig
        type t
        val to_yojson : t -> Yojson.Safe.t
        val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
        val default : t
    end

module type FI = functor (M : T) ->
    sig
      val write_internal : M.t -> string -> unit
      val read_internal : string -> M.t
      val replace_internal : string -> string -> unit
    end

module Make : FI

val read_config_tree : string -> Config_tree.t

val write_config_tree : Config_tree.t -> string -> unit

val read_reference_tree : string -> Reference_tree.t

val write_reference_tree : Reference_tree.t -> string -> unit
