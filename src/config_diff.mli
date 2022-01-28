type change = Unchanged | Added | Deleted | Updated of string list
type config_diff_data = change * Config_tree.config_node_data
type t = config_diff_data Vytree.t

type diff_func = string list -> change -> unit

type diff_trees = {
    left: Config_tree.t;
    right: Config_tree.t;
    add: Config_tree.t ref;
    del: Config_tree.t ref;
    inter: Config_tree.t ref;
}

exception Incommensurable
exception Empty_comparison

val make_diff_tree : Config_tree.t -> Config_tree.t -> diff_trees
val clone : ?with_children:bool -> 'a Vytree.t -> 'a Vytree.t -> string list -> 'a Vytree.t
val build_trees : diff_trees -> string list -> change -> unit
val compare : Config_tree.t -> Config_tree.t -> t
val get_add_compare : Config_tree.t -> Config_tree.t -> Config_tree.t

