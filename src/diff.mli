type change = Unchanged | Added | Subtracted | Updated of Config_tree.config_node_data

exception Incommensurable
exception Empty_comparison

module type Place = sig
    type t
    val diff_func : ?descent:bool -> string list -> t -> change -> t
end

module Diff : functor (P: Place) -> sig
    val diff_calc : string list -> P.t -> Config_tree.t option * Config_tree.t option -> P.t
    val diff : P.t -> Config_tree.t -> Config_tree.t -> P.t
end
