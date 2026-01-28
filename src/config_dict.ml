(** The goal here is to produce a JSON rendering to load into a Python dict.
 *)

(*let config_dict ?(mangle=false) ?(no_tag_mangle=false)*)

let hybrid_tree ?(with_first_node=true) ref_tree config_tree _mask path =
    let ct_at_path =
        Config_tree.get_subtree ~with_node:with_first_node config_tree path
    in
    let rt_at_path =
        Reference_tree.get_subtree ~with_node:with_first_node ref_tree path
    in
    let continue l =
    match l with
    | [] -> true
    | x :: _ -> x
    in
    let add_defaults ct p' =
        let ref_path =
            match with_first_node with
            | false -> Reference_tree.refpath rt_at_path p'
            | true -> Reference_tree.refpath rt_at_path (Util.drop_first p')
        in
        let relative_ref_tree = Reference_tree.get_subtree rt_at_path ref_path in
        let ref_tree_walk ((p, c), acc) node =
            let cont = continue c in
            if not cont then ((p, false::c), acc)
            else
            let rev_p = List.rev p in
            let sub_path = p' @ rev_p in
            (*
            if Util.is_empty sub_path
            then ((p, cont::c), acc)
            else
            if (Vytree.is_terminal_path[@alert "-exn"]) mask sub_path &&
               (Vytree.exists[@alert "-exn"]) ct sub_path
            then ((p, false::c), acc)
            else
            *)
            print_endline
            (Printf.sprintf "sub_path is %s; ref_path is %s" (Util.string_of_list sub_path)
            (Util.string_of_list ref_path));
            let data = Vytree.data_of_node node in
            match data.Reference_tree.node_type with
            | `Tag -> ((p, cont::c), acc)
            | `Leaf ->
                begin
                match data.default_value with
                | None -> ((p, cont::c), acc)
                | Some v ->
                    try
                    match data.multi with
                    | true ->
                        let acc' =
                            (Config_tree.set[@alert "-exn"]) acc sub_path (Some v) AddValue
                        in ((p, cont::c), acc')
                    | false ->
                        let acc' =
                            (Config_tree.set[@alert "-exn"]) acc sub_path (Some v) ReplaceValue
                        in ((p, cont::c), acc')
                    with Config_tree.Useless_set | Config_tree.Duplicate_value ->
                        ((p, cont::c), acc)
                end
            | _ -> ((p, cont::c), acc)
        in
        Vytree.fold_tree_with_path_cont ref_tree_walk (([], []), ct) relative_ref_tree
    in
    let config_tree_walk (p, acc) ct =
        let (data: Config_tree.config_node_data) = Vytree.data_of_node ct in
        if data.tag then (p, acc) else
        let rev_p = List.rev p in
        let ct' = add_defaults acc rev_p
        in (p, ct')
    in Vytree.fold_tree_with_path config_tree_walk ([], ct_at_path) ct_at_path


let config_dict _ref_tree ct =
    Config_tree.render_json ct
