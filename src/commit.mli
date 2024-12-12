
val drop_last_n: 'a List.t -> int -> 'a List.t

val fold_tree_depth: (string list * 'a -> 'b Vytree.t -> string list * 'a) -> string list * 'a -> 'b Vytree.t -> string list * 'a

val fold_tree_breadth: ('a -> 'b Vytree.t -> 'a) -> 'a -> 'b Vytree.t -> 'a
