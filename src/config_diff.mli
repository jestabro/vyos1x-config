type change = Unchanged | Added | Deleted | Updated of string list
type config_diff_data = change * Config_tree.config_node_data
type t = config_diff_data Vytree.t

exception Incommensurable
exception Empty_comparison

val clone : ?with_children:bool -> 'a Vytree.t -> 'a Vytree.t -> string list -> 'a Vytree.t
val compare : Config_tree.t -> Config_tree.t -> t
val get_add_compare : Config_tree.t -> Config_tree.t -> Config_tree.t

