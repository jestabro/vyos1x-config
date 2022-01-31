type change = Unchanged | Added | Deleted | Updated of string list
type config_diff_data = change * Config_tree.config_node_data
type t = config_diff_data Vytree.t

type diff_func = ?with_children:bool -> string list -> change -> unit

type diff_trees = {
    left: Config_tree.t;
    right: Config_tree.t;
    add: Config_tree.t ref;
    del: Config_tree.t ref;
    inter: Config_tree.t ref;
}

exception Incommensurable
exception Empty_comparison

let make_diff_tree l r = { left = l; right = r;
                           add = ref (Config_tree.make "root");
                           del = ref (Config_tree.make "root");
                           inter = ref (Config_tree.make "root");
}

let name_of n = Vytree.name_of_node n
let data_of n = Vytree.data_of_node n
let children_of n = Vytree.children_of_node n
let make data name children = Vytree.make_full data name children

let modify_data (m : change) (a : Config_tree.config_node_data)
                : config_diff_data = (m, a)
let modify_node m = Vytree.fmap (modify_data m)

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

let get_opt_name left_opt right_opt =
    match left_opt, right_opt with
    | Some left_node, None -> name_of left_node
    | None, Some right_node -> name_of right_node
    | Some left_node, Some _ -> name_of left_node
    | None, None -> raise Empty_comparison

let update_path path left_opt right_opt =
    let name = get_opt_name left_opt right_opt in
    if name = "root" then path
    else path @ [name]

let rec diff (path : string list) (f : diff_func) ((left_node_opt, right_node_opt) : Config_tree.t option * Config_tree.t option) : t =
    let path = update_path path left_node_opt right_node_opt in
    match left_node_opt, right_node_opt with
    | Some left_node, None -> (f path Deleted; delete_node left_node)
    | None, Some right_node -> (f path Added; add_node right_node)
    | Some left_node, Some right_node when left_node = right_node ->
            (f path Unchanged; keep_node left_node)
    | Some left_node, Some right_node when left_node ^~ right_node ->
            let values = (data_of right_node).values in
            (f path (Updated values); update_node values left_node)
    | Some left_node, Some right_node ->
            (f ~with_children:false path Unchanged;
            make (Unchanged, data_of left_node)
                 (name_of left_node)
                 (opt_zip left_node right_node |> List.map (diff path f)))
    | None, None -> raise Empty_comparison

(* copy node paths between trees *)
let rec clone_path ?(with_children=true) old_root new_root path_done path_remaining =
    match path_remaining with
    | [] | [_] ->
        let path_total = path_done @ path_remaining in
        let old_node = Vytree.get old_root path_total in
        if with_children then
            Vytree.insert ~children:(Vytree.children_of_node old_node) new_root path_total(Vytree.data_of_node old_node)
        else
            Vytree.insert new_root path_total(Vytree.data_of_node old_node)
    | name :: names ->
        let path_done = path_done @ [name] in
        let old_node = Vytree.get old_root path_done in
        let new_root = Vytree.insert new_root path_done (Vytree.data_of_node old_node) in
        clone_path ~with_children:with_children old_root new_root path_done names

let clone ?(with_children=true) old_root new_root path =
    let path_existing = Vytree.get_existent_path new_root path in
    let path_remaining = Vylist.complement path path_existing in
    clone_path ~with_children:with_children old_root new_root path_existing path_remaining

(* define the diff_func *)
let decorate_trees (trees : diff_trees) ?(with_children=true) (path : string list) (m : change) =
    match path with
    | [] -> ()
    | _ ->
            match m with
            | Added | Updated _ -> trees.add := clone trees.right !(trees.add) path
            | Deleted -> trees.del := clone ~with_children:false trees.left !(trees.del) path
            | Unchanged -> trees.inter := clone ~with_children:with_children trees.left !(trees.inter) path

let tree_at_path path node =
    let path = "root" :: path in
    try
        Vytree.get node path
    with Vytree.Nonexistent_path -> raise Empty_comparison

(* call recursive diff on config_trees with decorate_trees as the diff_func *)
let compare path left right =
    if (Vytree.name_of_node left) <> (Vytree.name_of_node right) then
        raise Incommensurable
    else
        let (left, right) = if (List.length path > 0) then
            (tree_at_path path left, tree_at_path path right) else (left, right) in
        let trees = make_diff_tree left right in
        let d = diff [] (decorate_trees trees) (Option.some left, Option.some right)
        in (trees, d)

let difference path left right =
    let trees, _ = compare path left right in
    (!(trees.add), !(trees.del), !(trees.inter))
