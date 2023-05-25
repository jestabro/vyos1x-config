type change = Unchanged | Added | Subtracted | Updated of string list

type diff_func = ?recurse:bool -> string list -> change -> unit

type diff_trees = {
    left: Config_tree.t;
    right: Config_tree.t;
    add: Config_tree.t ref;
    sub: Config_tree.t ref;
    del: Config_tree.t ref;
    inter: Config_tree.t ref;
}

module Diff_tree : sig
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               add: Config_tree.t;
               sub: Config_tree.t;
               del: Config_tree.t;
               inter: Config_tree.t;
             }
end

module Diff_string : sig
    type t = { ppath: string list;
               udiff: string;
             }
end

module Diff_cstore : sig
    type t = { handle: int; }
end

type _ result =
    | Diff_tree : Diff_tree.t -> Diff_tree.t result
    | Diff_string : Diff_string.t -> Diff_string.t result
    | Diff_cstore : Diff_cstore.t -> Diff_cstore.t result

type 'a diff_func_immut = ?recurse:bool -> string list -> 'a result -> change -> 'a result

exception Incommensurable
exception Empty_comparison
exception Nonexistent_child

val make_diff_trees : Config_tree.t -> Config_tree.t -> diff_trees
val clone : ?recurse:bool -> ?set_values:(string list) option -> Config_tree.t -> Config_tree.t -> string list -> Config_tree.t
val decorate_trees : diff_trees -> ?recurse:bool -> string list -> change -> unit
val compare : string list -> Config_tree.t -> Config_tree.t -> diff_trees
val diff_tree : string list -> Config_tree.t -> Config_tree.t -> Config_tree.t
val diff_tree_immut : string list -> Config_tree.t -> Config_tree.t -> Config_tree.t
val show_diff : ?cmds:bool -> string list -> Config_tree.t -> Config_tree.t -> string
val tree_union : Config_tree.t -> Config_tree.t -> Config_tree.t
