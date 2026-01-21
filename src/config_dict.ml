(** The goal here is to produce a JSON rendering to load into a Python dict.
 *)

(*let config_dict ?(mangle=false) ?(no_tag_mangle=false)*)

let hybrid_tree ?(with_first_node=true) ref_tree config_tree mask path =
    let ct_at_path =
        Config_tree.get_subtree ~with_node:with_first_node config_tree path
    in
    let add_defaults ct p' =
        let ref_path = Reference_tree.refpath ref_tree (path @ p') in
        let relative_ref_tree = Reference_tree.get_subtree ref_tree ref_path in
        let ref_tree_walk (p, (continue, acc)) node =
            if not continue
            then (p, (false, acc))
            else
            let rev_p = List.rev p in
            let total_path = p' @ rev_p in
            if Util.is_empty total_path
            then (p, (continue, acc))
            else
            if (Vytree.is_terminal_path[@alert "-exn"]) mask total_path &&
               (Vytree.exists[@alert "-exn"]) ct total_path
            then (p, (false, acc))
            else
            let data = Vytree.data_of_node node in
            match data.Reference_tree.node_type with
            | `Tag -> (p, (false, acc))
            | `Leaf ->
                begin
                match data.default_value with
                | None -> (p, (continue, acc))
                | Some v ->
                    match data.multi with
                    | true ->
                        let acc' =
                            (Config_tree.set[@alert "-exn"]) acc total_path (Some v) AddValue
                        in (p, (continue, acc'))
                    | false ->
                        let acc' =
                            (Config_tree.set[@alert "-exn"]) acc total_path (Some v) ReplaceValue
                        in (p, (continue, acc'))
                end
            | _ -> (p, (continue, acc))
        in
        let result =
            Vytree.fold_tree_with_path ref_tree_walk ([], (true, ct)) relative_ref_tree
        in snd result
    in
    let config_tree_walk (p, acc) _ct =
        let rev_p = List.rev p in
        let ct' = add_defaults acc rev_p
        in (p, ct')
    in Vytree.fold_tree_with_path config_tree_walk ([], ct_at_path) ct_at_path


let config_dict _ref_tree ct =
    Config_tree.render_json ct
