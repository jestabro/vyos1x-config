exception Incompatible_union
exception Nonexistent_child

module type T =
    sig
        type t

        val name_of : t -> string
        val children_of : t -> t list
        val find_child : t -> t -> t option
        val insert_child : t -> t -> t
        val replace_child : t -> t -> t
        val (^~) : t -> t -> bool
    end

module type TA = functor (M: T) ->
    sig
        val tree_union : M.t -> M.t -> (M.t -> M.t -> M.t) -> M.t
    end

module Make : TA = functor (M: T) -> struct
    module VytreeOrd = struct
        type t = M.t
        let compare a b =
            Util.lexical_numeric_compare (M.name_of a) (M.name_of b)
    end
    module ChildrenS = Set.Make(VytreeOrd)

    let union_of_children n m =
        let set_n = ChildrenS.of_list (M.children_of n) in
        let set_m = ChildrenS.of_list (M.children_of m) in
        ChildrenS.elements (ChildrenS.union set_n set_m)

    let rec tree_union s t f =
        if (M.name_of s) <> (M.name_of t) then
            raise Incompatible_union
        else
        let (^~) = M.(^~) in
        let child_of_union s t c =
            let s_c = M.find_child s c in
            let t_c = M.find_child t c in
            match s_c, t_c with
            | Some child, None ->
                M.insert_child t child
            | None, Some _ -> t
            | Some u, Some v ->
                    if (u ^~ v) then
                        M.replace_child t (tree_union u (f u v) f)
                    else
                        M.replace_child t (tree_union u v f)
            | None, None -> raise Nonexistent_child
        in
        List.fold_left (fun x c -> child_of_union s x c) t (union_of_children s t)
end
