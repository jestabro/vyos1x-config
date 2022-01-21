type change = Unchanged | Added | Deleted | Updated of string list
type config_diff_data = change * Config_tree.config_node_data
type t = config_diff_data Vytree.t

exception Incommensurable
exception Empty_comparison

let rec modify_node (m : change) (node : Config_tree.t) : t =
    Vytree.make_full (m, Vytree.data_of_node node)
                     (Vytree.name_of_node node)
                     (List.map (modify_node m) (Vytree.children_of_node node))
 
let add_node (node : Config_tree.t) : t =
    modify_node Added node
let delete_node (node : Config_tree.t) : t =
    modify_node Deleted node
let keep_node (node : Config_tree.t) : t =
    modify_node Unchanged node
let update_node values (node : Config_tree.t) : t =
    modify_node (Updated values) node

let (^~) (node : Config_tree.t) (node' : Config_tree.t) =
  (Vytree.name_of_node node) = (Vytree.name_of_node node') &&
  (Vytree.data_of_node node).values <> (Vytree.data_of_node node').values

let left_opt_pairs n m =
    (Vytree.children_of_node n) |> List.map (fun x ->
        let maybe_node =
            (Vytree.children_of_node m) |> List.find_opt (fun y ->
                Vytree.name_of_node y = Vytree.name_of_node x) in
        (Some x, maybe_node))

let right_opt_pairs n m =
    (Vytree.children_of_node m) |> List.map (fun y ->
        let maybe_node =
            (Vytree.children_of_node n) |> List.find_opt (fun x ->
                Vytree.name_of_node x = Vytree.name_of_node y) in
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
            update_node (Vytree.data_of_node right_node).values left_node
    | Some left_node, Some right_node ->
            Vytree.make_full (Unchanged, Vytree.data_of_node left_node)
                             (Vytree.name_of_node left_node)
                             (opt_zip left_node right_node |> List.map diff)
(* if vytree.t is not opaque, one can instead write as follows (and above),
   which is more legible:
      { name = Vytree.name_of_node left_node;
        data = (Unchanged, Vytree.data_of_node left_node);
        children = opt_zip left_node right_node |> List.map diff } *)
    | None, None -> raise Empty_comparison

let compare left right =
    if (Vytree.name_of_node left) <> (Vytree.name_of_node right) then
        raise Incommensurable
    else
        diff (Option.some left, Option.some right)

