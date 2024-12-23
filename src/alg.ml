exception Incompatible_union
exception Nonexistent_child

module type Data = sig
    type t
end

module type Dtree = sig
    module D: Data
    type t = D.t Vytree.t
end

module Dtree_impl (D: Data): Dtree with module D = D = struct
    module D = D
    type t = D.t Vytree.t
end

module Alg (D: Data) (T: Dtree with module D = D) = struct
    module TreeOrd = struct
        type t = T.t
        let compare a b =
            Util.lexical_numeric_compare (Vytree.name_of_node a) (Vytree.name_of_node b)
    end
    module ChildrenT = Set.Make(TreeOrd)

    let union_of_children n m =
        let set_n = ChildrenT.of_list (Vytree.children_of_node n) in
        let set_m = ChildrenT.of_list (Vytree.children_of_node m) in
        ChildrenT.elements (ChildrenT.union set_n set_m)

    let find_child n c = Vytree.find n (Vytree.name_of_node c)

    let insert_child n c = Vytree.insert ~position:Vytree.Lexical ~children:(Vytree.children_of_node c) n [(Vytree.name_of_node c)] (Vytree.data_of_node c)

    let replace_child n c =
        Vytree.replace n c

    let rec tree_union s t f =
        if (Vytree.name_of_node s) <> (Vytree.name_of_node t) then
            raise Incompatible_union
        else
        let child_of_union s t c =
            let s_c = find_child s c in
            let t_c = find_child t c in
            match s_c, t_c with
            | Some child, None ->
                insert_child t child
            | None, Some _ -> t
            | Some u, Some v ->
                    if (Vytree.data_of_node u <> Vytree.data_of_node v) then
                        replace_child t (tree_union u (f u v) f)
                    else
                        replace_child t (tree_union u v f)
            | None, None -> raise Nonexistent_child
        in
        List.fold_left (fun x c -> child_of_union s x c) t (union_of_children s t)
end

module ConfigData: Data with type t = Config_tree.config_node_data = struct
    type t = Config_tree.config_node_data
end

module RefData: Data with type t = Reference_tree.ref_node_data = struct
    type t = Reference_tree.ref_node_data
end

module ConfigAlg = Alg(ConfigData)(Dtree_impl(ConfigData))
module RefAlg = Alg(RefData)(Dtree_impl(RefData))
