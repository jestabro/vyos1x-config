(** The goal here is to produce a JSON rendering to load into a Python dict.
 *)

(*let config_dict ?(mangle=false) ?(no_tag_mangle=false)*)

let hybrid_tree ?(with_first_node=true) rt ct mask path =
    let ct_sub = Config_tree.get_subtree ~with_node:with_first_node ct path in
    let add_defaults ct p' =
        let rpath = Reference_tree.refpath rt (path @ p') in
        let rt_sub = Reference_tree.get_subtree rt rpath in
        let res =
            let fold_defaults (p, (continue, acc)) node =
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
            in Vytree.fold_tree_with_path fold_defaults ([], (true, ct)) rt_sub
        in snd res
    in
    let ct_func (p, acc) _c =
        let rev_p = List.rev p in
        let c' = add_defaults acc rev_p
        in (p, c')
        (*
        if Vytree.is_terminal_path c p then
            let c' = add_defaults acc p
            in (p, c')
        else (p, acc)
        *)
    in Vytree.fold_tree_with_path ct_func ([], ct_sub) ct_sub


let config_dict _rt ct =
    Config_tree.render_json ct
