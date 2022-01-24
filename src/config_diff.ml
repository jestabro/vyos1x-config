type change = Unchanged | Added | Deleted | Updated of string list
type config_diff_data = change * Config_tree.config_node_data
type t = config_diff_data Vytree.t

exception Incommensurable
exception Empty_comparison

let name_of n = Vytree.name_of_node n
let data_of n = Vytree.data_of_node n
let children_of n = Vytree.children_of_node n
let make data name children = Vytree.make_full data name children

(*
let get_change d = fst d
let get_data d = snd d
*)
(*let rec modify_node (m : change) (node : Config_tree.t) : t =
    make (m, data_of node)
         (name_of node)
         (List.map (modify_node m) (children_of node))
*)
let modify_data (m : change) (a : Config_tree.config_node_data)
                : config_diff_data = (m, a)
let modify_node m = Vytree.fmap (modify_data m)
(*
let rec forget (node: t) : Config_tree.t =
    make (get_config_node_data node)
         (name_of node)
         (List.map forget (children_of node))
*) 
let add_node (node : Config_tree.t) : t =
    modify_node Added node
let delete_node (node : Config_tree.t) : t =
    modify_node Deleted node
let keep_node (node : Config_tree.t) : t =
    modify_node Unchanged node
let update_node values (node : Config_tree.t) : t =
    modify_node (Updated values) node

let (^~) (node : Config_tree.t) (node' : Config_tree.t) =
  name_of node = name_of node' &&
  (data_of node).values <> (data_of node').values

let left_opt_pairs n m =
    (children_of n) |> List.map (fun x ->
        let maybe_node =
            (children_of m) |> List.find_opt (fun y ->
                name_of y = name_of x) in
        (Some x, maybe_node))

let right_opt_pairs n m =
    (children_of m) |> List.map (fun y ->
        let maybe_node =
            (children_of n) |> List.find_opt (fun x ->
                name_of x = name_of y) in
        (maybe_node, Some y))

let opt_zip n m =
    left_opt_pairs n m @ right_opt_pairs n m |> List.sort_uniq compare

let rec diff ((left_node_opt, right_node_opt) : Config_tree.t option * Config_tree.t option) : t =
    match left_node_opt, right_node_opt with
    | Some left_node, None -> delete_node left_node
    | None, Some right_node -> add_node right_node
    | Some left_node, Some right_node when left_node = right_node ->
            keep_node left_node
    | Some left_node, Some right_node when left_node ^~ right_node ->
            update_node (data_of right_node).values left_node
    | Some left_node, Some right_node ->
            make (Unchanged, data_of left_node)
                 (name_of left_node)
                 (opt_zip left_node right_node |> List.map diff)
    | None, None -> raise Empty_comparison

let compare left right =
    if (Vytree.name_of_node left) <> (Vytree.name_of_node right) then
        raise Incommensurable
    else
        diff (Option.some left, Option.some right)
(*
let filter_add (d : config_diff_data) : Config_tree.config_node_data option =
    match (get_change d) with
    | Unchanged | Added -> Some (get_data d)
    | _ -> None
let get_add_tree t = (Vytree.filter_fmap filter_add) t
*)
(*
let get_add_compare (left: Config_tree.t) (right : Config_tree.t) : Config_tree.t =
    let diff_tree = (compare left right) in
    Option.value (get_add_tree diff_tree) default=(Config_tree.t make "root")
*)
(*in let get_added_list = List.filter_map fmap filter_add 
in let rec get_added_tree t list : Config_tree.t list =*)


(* let get_added_config (d : t) : Config_tree.t =
    let rec added_config =
        match get_change d with
        | Unchanged | Added ->
                make (forget data_of d)
                     (name_of d)
                     (List.map added_config (children_of d))
        | _ -> empty tree  add variant *)
