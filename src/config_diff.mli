type change = Unchanged | Added | Deleted | Updated of string list
type config_diff_data = change * Config_tree.config_node_data
type t = config_diff_data Vytree.t

type diff_func = ?with_children:bool -> string list -> change -> unit

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
val decorate_trees : diff_trees -> ?with_children:bool -> string list -> change -> unit
val compare : string list -> Config_tree.t -> Config_tree.t -> diff_trees * t
val diffs : string list -> Config_tree.t -> Config_tree.t -> Config_tree.t * Config_tree.t * Config_tree.t

