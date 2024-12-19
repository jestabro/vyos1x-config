
type tree_data = Config of Config_tree.config_node_data | Ref of Reference_tree.ref_node_data

exception Incompatible_union
exception Nonexistent_child

module TreeOrd = struct
    type t = tree_data Vytree.t
    let compare a b =
        Util.lexical_numeric_compare (Vytree.name_of_node a) (Vytree.name_of_node b)
end
module ChildrenS = Set.Make(TreeOrd)

let union_of_children n m =
    let set_n = ChildrenS.of_list (Vytree.children_of_node n) in
    let set_m = ChildrenS.of_list (Vytree.children_of_node m) in
    ChildrenS.elements (ChildrenS.union set_n set_m)

let rec tree_union f s t =
        if (Vytree.name_of_node s) <> (Vytree.name_of_node t) then
            raise Incompatible_union
        else
    let child_of_union s t c =
        let s_c = Vytree.find s (Vytree.name_of_node c) in
        let t_c = Vytree.find t (Vytree.name_of_node c) in
        match s_c, t_c with
        | Some c, None ->
            Vytree.insert ~position:Lexical ~children:(Vytree.children_of_node c) t [(Vytree.name_of_node c)] (Vytree.data_of_node c)
        | None, Some _ -> t
        | Some u, Some v ->
                if Vytree.data_of_node u <> Vytree.data_of_node v then
                    let data = f u v in
                    Vytree.replace t (Vytree.make data (Vytree.name_of_node v))
                else
                    Vytree.replace t (tree_union u v)
        | None, None -> raise Nonexistent_child
    in
    List.fold_left (fun x c -> child_of_union s x c) t (union_of_children s t)
