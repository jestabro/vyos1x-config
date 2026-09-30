open Diff

module ValueS = Config_tree.ValueS

module Diff_tree = struct
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               add: Config_tree.t;
               sub: Config_tree.t;
               del: Config_tree.t;
               inter: Config_tree.t;
               diff_comments: bool;
             }

    let make_init ?(diff_comments=false) l r =
        { left = l;
          right = r;
          add = Config_tree.default;
          sub = Config_tree.default;
          del = Config_tree.default;
          inter = Config_tree.default;
          diff_comments = diff_comments;
        }

    let diff_func ?(descent=true) (path : string list) res (m : change) =
        (* raises no exception:
            clone will always be called on extant path of left or right
           alert exn Vytree.get_values:
            [Vytree.Empty_path] not possible as pattern Updated implies non-empty path
            [Vytree.Nonexistent_path] not possible as pattern Updated implies path exists
         *)
        match m with
        | Added -> {res with add = (Config_tree.clone[@alert "-exn"]) res.right res.add path; }
        | Subtracted ->
            {res with sub = (Config_tree.clone[@alert "-exn"]) res.left res.sub path;
             del = (Config_tree.clone[@alert "-exn"]) ~descent:false ~set_values:(Some []) res.left res.del path; }
        | Unchanged ->
            {res with inter = (Config_tree.clone[@alert "-exn"]) ~descent:descent res.left res.inter path; }
        | Updated (ldata, rdata) ->
            (* if in this case, node at path is guaranteed to exist *)
            (*  *)
            let diff_comments = res.diff_comments in
            (*let comm_op =
                match diff_comments with
                | true -> Config_tree.Copy
                | false -> Drop
            in*)
            (*let inter_comm_op =
                match diff_comments, (ldata.comment <> rdata.comment) with
                | true, true -> Config_tree.Drop
                | true, false -> Copy
                | false, _ -> Drop
            in*)
            let inter_comm_op =
                match (ldata.comment <> rdata.comment) with
                | true -> Config_tree.Drop
                | false -> Copy
            in
            let comment_diff = (ldata.comment <> rdata.comment) && diff_comments in
            let sub_comment = comment_diff && Option.is_some ldata.comment in
            let add_comment = comment_diff && Option.is_some rdata.comment in
            let option_is_empty o =
                let o' = Option.value ~default:[] o in
                Util.is_empty o'
            in
            (* collect actionable values *)
            let default_values = (None, None, None) in
            let sub_vals_opt, add_vals_opt, inter_vals_opt =
                if not ldata.leaf then
                    default_values
                else
                let v = rdata.values in
                let ov = ldata.values in
                if ov = v then default_values
                else
                let ov_set = ValueS.of_list ov in
                let v_set = ValueS.of_list v in
                let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                let inter_vals = ValueS.elements (ValueS.inter ov_set v_set) in
                (Some sub_vals, Some add_vals, Some inter_vals)
            in
            let values = (sub_vals_opt, add_vals_opt, inter_vals_opt) in
            if not diff_comments && values = default_values then
                (* for example, if diff_comments = false in a non-leaf node,
                   despite the fact that we are in case Updated *)
                res
            else
            let data_clone = (Config_tree.clone[@alert "-exn"]) ~descent:false in
            let sub_tree =
                if Option.is_none sub_vals_opt then
                    if sub_comment then
                        data_clone ~set_values:sub_vals_opt res.left res.sub path
                    else
                        res.sub
                else
                    if not (option_is_empty sub_vals_opt) || sub_comment then
                        data_clone ~set_values:sub_vals_opt res.left res.sub path
                    else
                        res.sub
            in
            let del_tree =
                (* check: do we want to include comments here ? *)
                if Option.is_none sub_vals_opt then
                    res.del
                else
                    if not (option_is_empty sub_vals_opt) then
                        if (option_is_empty add_vals_opt) && (option_is_empty inter_vals_opt) then
                            (* delete whole node, not just values *)
                            data_clone ~set_values:(Some []) ~comments:Drop res.left res.del path
                        else
                            data_clone ~set_values:sub_vals_opt ~comments:Drop res.left res.del path
                    else
                        res.del
            in
            let add_tree =
                if Option.is_none add_vals_opt then
                    if add_comment then
                        data_clone ~set_values:add_vals_opt res.right res.add path
                    else
                        res.add
                else
                    if not (option_is_empty add_vals_opt) || add_comment then
                        data_clone ~set_values:add_vals_opt res.right res.add path
                    else
                        res.add
            in
            let inter_tree =
                (*if Option.is_none inter_vals_opt then
                    if diff_comments then
                        data_clone ~set_values:inter_vals_opt ~comments:inter_comm_op res.left res.inter path
                    else
                        res.inter
                else*)
                    if not (option_is_empty inter_vals_opt) then
                        data_clone ~set_values:inter_vals_opt ~comments:inter_comm_op res.left res.inter path
                    else
                        res.inter
            in
            { res with add = add_tree;
              sub = sub_tree;
              del = del_tree;
              inter = inter_tree; }

(*
                match ov, v with
                | [_], [_] ->


                match ov, v with
                | [_], [_] -> {res with sub = (Config_tree.clone[@alert "-exn"]) res.left res.sub path;
                               del = (Config_tree.clone[@alert "-exn"]) res.left res.del path;
                               add = (Config_tree.clone[@alert "-exn"]) res.right res.add path; }
                | _, _ -> let ov_set = ValueS.of_list ov in
                          let v_set = ValueS.of_list v in
                          let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                          let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                          let inter_vals = ValueS.elements (ValueS.inter ov_set v_set) in
                          let sub_tree =
                              if not (Util.is_empty sub_vals) then
                                  (Config_tree.clone[@alert "-exn"]) ~set_values:(Some sub_vals) res.left res.sub path
                              else
                                  res.sub
                          in
                          let del_tree =
                              if not (Util.is_empty sub_vals) then
                                  if (Util.is_empty add_vals) && (Util.is_empty inter_vals) then
                                      (* delete whole node, not just values *)
                                      (Config_tree.clone[@alert "-exn"]) ~set_values:(Some []) res.left res.del path
                                  else
                                      (Config_tree.clone[@alert "-exn"]) ~set_values:(Some sub_vals) res.left res.del path
                              else
                                  res.del
                          in
                          let add_tree =
                              if not (Util.is_empty add_vals) then
                                  (Config_tree.clone[@alert "-exn"]) ~set_values:(Some add_vals) res.right res.add path
                              else
                                  res.add
                          in
                          let inter_tree =
                              if not (Util.is_empty inter_vals) then
                                  (Config_tree.clone[@alert "-exn"]) ~set_values:(Some inter_vals) res.left res.inter path
                              else
                                  res.inter
                          in { res with add = add_tree;
                               sub = sub_tree;
                               del = del_tree;
                               inter = inter_tree; }
*)
end

module D = Diff(Diff_tree)

(* get sub trees for path-relative comparison *)

let tree_at_path path node =
    (* raises:
        [Vytree.Empty_path]
        [Empty_comparison]
       alert exn Vytree.get:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] catch and raise Empty_comparison
     *)
    try
        let node = (Vytree.get[@alert "-exn"]) node path in
        Vytree.make_full Config_tree.default_data "" [node]
    with Vytree.Nonexistent_path -> raise Empty_comparison

(* call recursive diff on Diff_tree.t with Diff_tree.diff_func *)

let diff_trees ?(diff_comments=false) path left right =
    (* raises:
        [Empty_comparison] from tree_at_path
        [Incommensurable]
     *)
    if (Vytree.name_of_node left) <> (Vytree.name_of_node right) then
        raise Incommensurable
    else
        let (left, right) = if not (path = []) then
            (tree_at_path path left, tree_at_path path right) else (left, right) in
        let trees = Diff_tree.make_init ~diff_comments left right in
        D.diff trees left right

(* wrapper to return single tree with diff trees as subtrees *)

let diff_tree ?(diff_comments=false) path left right =
    (* raises:
        [Incommensurable],
        [Empty_comparison] from compare
     *)
    let trees = diff_trees ~diff_comments path left right in
    let add_node =
        Vytree.make_full Config_tree.default_data "add" (Vytree.children_of_node (trees.add)) in
    let sub_node =
        Vytree.make_full Config_tree.default_data "sub" (Vytree.children_of_node (trees.sub)) in
    let del_node =
        Vytree.make_full Config_tree.default_data "del" (Vytree.children_of_node (trees.del)) in
    let int_node =
        Vytree.make_full Config_tree.default_data "inter" (Vytree.children_of_node (trees.inter)) in
    Vytree.make_full Config_tree.default_data "" [add_node; sub_node; del_node; int_node]

(* convenience function needed for commit algorithm:
    we need a hybrid tree between the 'del' tree and the 'sub' tree, namely:
    in case the del tree has a terminal tag node (== all tag values have
    been removed) add tag node values for proper removal in commit execution
 *)

let get_tagged_delete_tree dt =
    (* alert exn Config_tree.is_tag:
        [Vytree.Empty_path] not possible in pattern non-empty path
        [Vytree.Nonexistent_path] not possible in fold_tree_with_path
       alert exn Vytree.is_terminal_path:
        [Vytree.Empty_path] not possible in pattern non-empty path
       alert exn Vytree.children_of_path:
        [Vytree.Empty_path] not possible in pattern non-empty path
        [Vytree.Nonexistent_path] not possible in super-tree of fold_tree_with_path arg
       alert exn Vytree.insert:
        [Vytree.Empty_path]: not possible since called on pattern path non-empty
        [Not_found]: not possible for postion=Lexical
        [Vytree.Duplicate_child]: not possible by condition is_terminal_path
        [Vytree.Insert_error]: not possible since constructed iteratively from existing path
     *)
    let del_tree = Config_tree.get_subtree dt ["del"] in
    let sub_tree = Config_tree.get_subtree dt ["sub"] in
    let f (p, a) _t =
        let q = List.rev p in
        match q with
        | [] -> (p, a)
        | _ ->
        if (Config_tree.is_tag[@alert "-exn"]) a q && (Vytree.is_terminal_path[@alert "-exn"]) a q then
            let children = (Vytree.children_of_path[@alert "-exn"]) sub_tree q in
            let insert_child path node name =
                (Vytree.insert[@alert "-exn"]) ~position:Lexical node (path @ [name]) Config_tree.default_data
            in
            let a' = List.fold_left (insert_child q) a children in
            (p, a')
        else
            (p, a)
    in
    Vytree.fold_tree_with_path f ([], del_tree) del_tree
