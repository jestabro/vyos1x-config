module type TYPE_PARAM =
    sig
        type u
    end

module type V : TYPE_PARAM =
    sig
        type t = TYPE_PARAM.u Vytree.t
        val compare : t -> t -> int
(*    let compare a b =
        Util.lexical_numeric_compare (Vytree.name_of_node a)
(Vytree.name_of_node b)*)
end
(*
module type VA = functor (M: V) ->
    sig
        val tree_union : M.t -> M.t -> M.t
    end

module Make : VA = functor (M: V) -> struct
    let tree_union a b = b
end
*)
