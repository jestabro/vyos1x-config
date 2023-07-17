type change = Unchanged | Added | Subtracted | Updated of string list

exception Incommensurable
exception Empty_comparison of string list
exception Nonexistent_child

(* temp comment: begin transition to immutable structure *)
module Diff_tree = struct
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               add: Config_tree.t;
               sub: Config_tree.t;
               del: Config_tree.t;
               inter: Config_tree.t;
             }
end

module Diff_string = struct
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               add: Config_tree.t;
               sub: Config_tree.t;
               ppath: string list;
               udiff: string;
             }
end

module Diff_cstore = struct
    type t = { handle: int; }
end

type _ result =
    | Diff_tree : Diff_tree.t -> Diff_tree.t result
    | Diff_string : Diff_string.t -> Diff_string.t result
    | Diff_cstore : Diff_cstore.t -> Diff_cstore.t result

let eval_result : type a. a result -> a = function
    | Diff_tree x -> x
    | Diff_string x -> x
    | Diff_cstore x -> x

type 'a diff_func_immut = ?recurse:bool -> string list -> 'a result -> change -> 'a result

let make_diff_trees_immut l r = Diff_tree { left = l; right = r;
                                  add = (Config_tree.make "");
                                  sub = (Config_tree.make "");
                                  del = (Config_tree.make "");
                                  inter = (Config_tree.make "");
}

let make_diff_string l r = Diff_string {
                               left = l; right = r;
                               add = (Config_tree.make "");
                               sub = (Config_tree.make "");
                               ppath = [];
                               udiff = "";
                           }

(* original mutable structure still in use *)
type diff_trees = {
    left: Config_tree.t;
    right: Config_tree.t;
    add: Config_tree.t ref;
    sub: Config_tree.t ref;
    del: Config_tree.t ref;
    inter: Config_tree.t ref;
}

type diff_func = ?recurse:bool -> string list -> change -> unit

let make_diff_trees l r = { left = l; right = r;
                           add = ref (Config_tree.make "");
                           sub = ref (Config_tree.make "");
                           del = ref (Config_tree.make "");
                           inter = ref (Config_tree.make "");
}

let name_of n = Vytree.name_of_node n
let data_of n = Vytree.data_of_node n
let children_of n = Vytree.children_of_node n
let make data name children = Vytree.make_full data name children

module ValueOrd = struct
    type t = string
    let compare a b =
        Util.lexical_numeric_compare a b
end
module ValueS = Set.Make(ValueOrd)

module TreeOrd = struct
    type t = Config_tree.t
    let compare a b =
        Util.lexical_numeric_compare (name_of a) (name_of b)
end
module ChildrenS = Set.Make(TreeOrd)

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

(* this is module option 'compare', but with Some _ preceding None, which is
   useful for maintaing left-right -> top-down order for show_diff
 *)
let opt_cmp o0 o1 =
    match o0, o1 with
    | Some v0, Some v1 -> compare v0 v1
    | None, None -> 0
    | None, Some _ -> 1
    | Some _, None -> -1

let tuple_cmp t1 t2 =
    match t1, t2 with
    | (x1, y1), (x2, y2) ->
            let first = opt_cmp x1 x2 in
            if first <> 0 then first else opt_cmp y1 y2

let opt_zip n m =
    left_opt_pairs n m @ right_opt_pairs n m |> List.sort_uniq tuple_cmp

let get_opt_name left_opt right_opt =
    match left_opt, right_opt with
    | Some left_node, None -> name_of left_node
    | None, Some right_node -> name_of right_node
    | Some left_node, Some _ -> name_of left_node
    | None, None -> raise (Empty_comparison ["get_opt_name"])

let update_path path left_opt right_opt =
    let name = get_opt_name left_opt right_opt in
    if name = "" then path
    else path @ [name]

(* tree diff algorithm: walk the tree pair, calling a function of type
   diff_func for each comparison.
   The idea of matching on pairs of (node opt) is from
   https://github.com/LukeBurgessYeo/tree-diff
 *)
let rec diff (path : string list) (f : diff_func) (l : (Config_tree.t option * Config_tree.t option) list) =
    match l with
    | [] -> ()
    | (left_node_opt, right_node_opt) :: ls ->
        (let path = update_path path left_node_opt right_node_opt in
        match left_node_opt, right_node_opt with
        | Some _, None -> f path Subtracted
        | None, Some _ -> f path Added
        | Some left_node, Some right_node when left_node = right_node ->
                f path Unchanged
        | Some left_node, Some right_node when left_node ^~ right_node ->
                let values = (data_of right_node).values in
                f path (Updated values)
        | Some left_node, Some right_node ->
                (f ~recurse:false path Unchanged;
                diff path f (opt_zip left_node right_node))
        | None, None -> raise (Empty_comparison path))
        ; diff path f ls

let rec diff_immut (path : string list) (res: 'a result) (f : 'a diff_func_immut) (l : (Config_tree.t option * Config_tree.t option) list) =
    match l with
    | [] -> res
    | (left_node_opt, right_node_opt) :: ls ->
        let res =
        (let path = update_path path left_node_opt right_node_opt in
            match left_node_opt, right_node_opt with
            | Some _, None -> f path res Subtracted
            | None, Some _ -> f path res Added
            | Some left_node, Some right_node when left_node = right_node ->
                    f path res Unchanged
            | Some left_node, Some right_node when left_node ^~ right_node ->
                    let values = (data_of right_node).values in
                    f path res (Updated values)
            | Some left_node, Some right_node ->
                    let ret = f ~recurse:false path res Unchanged
                    in diff_immut path ret f (opt_zip left_node right_node)
            | None, None ->
                    let a = ["diff_immut"] @ path in raise (Empty_comparison a))
        in diff_immut path res f ls

(* copy node paths between trees *)
let rec clone_path ?(recurse=true) ?(set_values=None) old_root new_root path_done path_remaining =
    match path_remaining with
    | [] | [_] ->
        let path_total = path_done @ path_remaining in
        let old_node = Vytree.get old_root path_total in
        let data =
            match set_values with
            | Some v -> { (data_of old_node) with Config_tree.values = v }
            | None -> data_of old_node
        in
        if recurse then
            Vytree.insert ~position:Lexical ~children:(children_of old_node) new_root path_total data
        else
            Vytree.insert ~position:Lexical new_root path_total data
    | name :: names ->
        let path_done = path_done @ [name] in
        let old_node = Vytree.get old_root path_done in
        let new_root = Vytree.insert ~position:Lexical new_root path_done (data_of old_node) in
        clone_path ~recurse:recurse ~set_values:set_values old_root new_root path_done names

let clone ?(recurse=true) ?(set_values=None) old_root new_root path =
    match path with
    | [] -> if recurse then old_root else new_root
    | _ ->
            let path_existing = Vytree.get_existent_path new_root path in
            let path_remaining = Vylist.complement path path_existing in
            clone_path ~recurse:recurse ~set_values:set_values old_root new_root path_existing path_remaining

let is_empty l = (l = [])

(* define the diff_func; in this instance, we imperatively build the difference trees *)
let decorate_trees (trees : diff_trees) ?(recurse=true) (path : string list) (m : change) =
    match m with
    | Added -> trees.add := clone trees.right !(trees.add) path
    | Subtracted -> trees.sub := clone trees.left !(trees.sub) path;
                    trees.del := clone ~recurse:false ~set_values:(Some []) trees.left !(trees.del) path
    | Unchanged -> trees.inter := clone ~recurse:recurse trees.left !(trees.inter) path
    | Updated v ->
            (* if in this case, node at path is guaranteed to exist *)
            let ov = Config_tree.get_values trees.left path in
            match ov, v with
            | [_], [_] -> trees.sub := clone trees.left !(trees.sub) path;
                          trees.del := clone trees.left !(trees.del) path;
                          trees.add := clone trees.right !(trees.add) path
            | _, _ -> let ov_set = ValueS.of_list ov in
                      let v_set = ValueS.of_list v in
                      let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                      let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                      let inter_vals = ValueS.elements (ValueS.inter ov_set v_set) in
                      if not (is_empty sub_vals) then
                          trees.sub := clone ~set_values:(Some sub_vals) trees.left !(trees.sub) path;
                      if not (is_empty sub_vals) then
                          if (is_empty add_vals) && (is_empty inter_vals) then
                              (* delete whole node, not just values *)
                              trees.del := clone ~set_values:(Some []) trees.left !(trees.del) path
                          else
                              trees.del := clone ~set_values:(Some sub_vals) trees.left !(trees.del) path;
                      if not (is_empty add_vals) then
                          trees.add := clone ~set_values:(Some add_vals) trees.right !(trees.add) path;
                      if not (is_empty inter_vals) then
                          trees.inter := clone ~set_values:(Some inter_vals) trees.left !(trees.inter) path

(* define the diff_func_immut *)
let decorate_trees_immut ?(recurse=true) (path : string list) (Diff_tree res) (m : change) =
    match m with
    | Added -> Diff_tree {res with add = clone res.right res.add path }
    | Subtracted ->
        Diff_tree {res with sub = clone res.left res.sub path;
         del = clone ~recurse:false ~set_values:(Some []) res.left res.del path }
    | Unchanged ->
        Diff_tree {res with inter = clone ~recurse:recurse res.left res.inter path }
    | Updated v ->
            (* if in this case, node at path is guaranteed to exist *)
            let ov = Config_tree.get_values res.left path in
            match ov, v with
            | [_], [_] -> Diff_tree {res with sub = clone res.left res.sub path;
                           del = clone res.left res.del path;
                           add = clone res.right res.add path }
            | _, _ -> let ov_set = ValueS.of_list ov in
                      let v_set = ValueS.of_list v in
                      let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                      let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                      let inter_vals = ValueS.elements (ValueS.inter ov_set v_set) in
                      let sub_tree =
                          if not (is_empty sub_vals) then
                              clone ~set_values:(Some sub_vals) res.left res.sub path
                          else
                              res.sub
                      in
                      let del_tree =
                          if not (is_empty sub_vals) then
                              if (is_empty add_vals) && (is_empty inter_vals) then
                                  (* delete whole node, not just values *)
                                  clone ~set_values:(Some []) res.left res.del path
                              else
                                  clone ~set_values:(Some sub_vals) res.left res.del path
                          else
                              res.del
                      in
                      let add_tree =
                          if not (is_empty add_vals) then
                            clone ~set_values:(Some add_vals) res.right res.add path
                          else
                              res.add
                      in
                      let inter_tree =
                          if not (is_empty inter_vals) then
                              clone ~set_values:(Some inter_vals) res.left res.inter path
                          else
                              res.inter
                      in Diff_tree { res with add = add_tree;
                           sub = sub_tree;
                           del = del_tree;
                           inter = inter_tree; }

(* get sub trees for path-relative comparison *)
let tree_at_path path node =
    try
        let node = Vytree.get node path in
        make Config_tree.default_data "" [node]
    with Vytree.Nonexistent_path ->
        let a = ["tree_at_path"] @ path in raise (Empty_comparison a)

(* call recursive diff on config_trees with decorate_trees as the diff_func *)
let compare path left right =
    if (name_of left) <> (name_of right) then
        raise Incommensurable
    else
        let (left, right) = if not (path = []) then
            (tree_at_path path left, tree_at_path path right) else (left, right) in
        let trees = make_diff_trees left right in
        diff [] (decorate_trees trees) [(Option.some left, Option.some right)];
        trees

(* wrapper to return diff trees *)
let diff_tree path left right =
    let trees = compare path left right in
    let add_node = make Config_tree.default_data "add" (children_of !(trees.add)) in
    let sub_node = make Config_tree.default_data "sub" (children_of !(trees.sub)) in
    let del_node = make Config_tree.default_data "del" (children_of !(trees.del)) in
    let int_node = make Config_tree.default_data "inter" (children_of !(trees.inter)) in
    let ret = make Config_tree.default_data "" [add_node; sub_node; del_node; int_node] in
    ret

(* call recursive diff on config_trees with decorate_trees as the diff_func *)
let compare_immut path left right =
    if (name_of left) <> (name_of right) then
        raise Incommensurable
    else
        let (left, right) = if not (path = []) then
            (tree_at_path path left, tree_at_path path right) else (left, right) in
        let trees = make_diff_trees_immut left right in
        let d = diff_immut [] trees (decorate_trees_immut) [(Option.some left, Option.some right)]
        in eval_result d

(* wrapper to return diff trees *)
let diff_tree_immut path left right =
    let trees = compare_immut path left right in
    let add_node = make Config_tree.default_data "add" (children_of (trees.add)) in
    let sub_node = make Config_tree.default_data "sub" (children_of (trees.sub)) in
    let del_node = make Config_tree.default_data "del" (children_of (trees.del)) in
    let int_node = make Config_tree.default_data "inter" (children_of (trees.inter)) in
    let ret = make Config_tree.default_data "" [add_node; sub_node; del_node; int_node] in
    ret

(* the following builds a diff_func to return a unified diff string of
   configs or config commands
 *)
let list_but_last l =
    let len = List.length l in
    List.filteri (fun i _ -> i < len - 1) l

let ppath_to_string_if_new (c: string list ref) (path: string list) =
    let p = list_but_last path in
    if (!c <> p) then
        (c := p; Printf.sprintf "[%s]\n" (String.concat " " !c)) else ""

let ppath_string_if_new (c: string list) (path: string list) =
    let p = list_but_last path in
    if (c <> p) then
        Printf.sprintf "[%s]\n" (String.concat " " c) else ""

let marked_render mark node =
    let lines = Config_tree.render_config node in
    let l = String.split_on_char '\n' lines in
    let m =
        List.map (fun s -> if (String.length s) > 0 then mark ^ s else s) l in
    String.concat "\n" m

let added_lines ?(cmds=false) node path =
    if not cmds then marked_render "+ " (tree_at_path path node)
    else
        let skel = Config_tree.make "" in
        let snode = clone node skel path in
        (Config_tree.render_commands ~op:Set snode []) ^ "\n"

let removed_lines ?(cmds=false) node path =
    if not cmds then marked_render "- " (tree_at_path path node)
    else
        let skel = Config_tree.make "" in
        let snode = clone node skel path in
        (Config_tree.render_commands ~op:Delete snode []) ^ "\n"

let order_commands (strl: string ref) =
    let l = String.split_on_char '\n' !strl in
    let del = List.filter (fun s -> (s <> "") && (s.[0] = 'd')) l in
    let set = List.filter (fun s -> (s <> "") && (s.[0] = 's')) l in
    strl := (String.concat "\n" del) ^ "\n" ^ (String.concat "\n" set) ^ "\n"

let order_commands_immut (strl: string) =
    let l = String.split_on_char '\n' strl in
    let del = List.filter (fun s -> (s <> "") && (s.[0] = 'd')) l in
    let set = List.filter (fun s -> (s <> "") && (s.[0] = 's')) l in
    (String.concat "\n" del) ^ "\n" ^ (String.concat "\n" set) ^ "\n"

let ppath = ref [""]

let unified_diff ?(cmds=false) (str_diff: string ref) (trees : diff_trees) ?recurse:_ (path : string list) (m : change) =
    match m with
    | Added ->
            if not cmds then str_diff := !str_diff ^ (ppath_to_string_if_new ppath path);
            str_diff := !str_diff ^ (added_lines ~cmds:cmds trees.right path)
    | Subtracted ->
            if not cmds then str_diff := !str_diff ^ (ppath_to_string_if_new ppath path);
            str_diff := !str_diff ^ (removed_lines ~cmds:cmds trees.left path)
    | Unchanged -> ()
    | Updated v ->
            if not cmds then str_diff := !str_diff ^ (ppath_to_string_if_new ppath path);
            let ov = Config_tree.get_values trees.left path in
            match ov, v with
            | [_], [_] ->
                    str_diff := !str_diff ^ (removed_lines ~cmds:cmds trees.left path);
                    str_diff := !str_diff ^ (added_lines ~cmds:cmds trees.right path)
            | _, _ -> let ov_set = ValueS.of_list ov in
                      let v_set = ValueS.of_list v in
                      let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                      let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                      if not (is_empty sub_vals) then
                          (trees.sub := clone ~set_values:(Some sub_vals) trees.left !(trees.sub) path;
                           str_diff := !str_diff ^ (removed_lines ~cmds:cmds !(trees.sub) path));
                      if not (is_empty add_vals) then
                          (trees.add := clone ~set_values:(Some add_vals) trees.right !(trees.add) path;
                           str_diff := !str_diff ^ (added_lines ~cmds:cmds !(trees.add) path))

let unified_diff_immut ?(cmds=false) ?recurse:_ (path : string list) (Diff_string res) (m : change) =
    let ppath_s = ppath_string_if_new res.ppath path in
    let str_diff =
        if not cmds then
            res.udiff ^ ppath_s
        else
            res.udiff
    in
    let ppath_l =
        if (ppath_s <> "") then
            path
        else
            res.ppath
    in
    match m with
    | Added ->
            let str_diff =
                str_diff ^ (added_lines ~cmds:cmds res.right path)
            in
            Diff_string { res with ppath = ppath_l; udiff = str_diff; }
    | Subtracted ->
            let str_diff =
                str_diff ^ (removed_lines ~cmds:cmds res.right path)
            in
            Diff_string { res with ppath = ppath_l; udiff = str_diff; }
    | Unchanged -> Diff_string { res with ppath = ppath_l; }
    | Updated v ->
            let ov = Config_tree.get_values res.left path in
            match ov, v with
            | [_], [_] ->
                    let str_diff =
                        str_diff ^ (removed_lines ~cmds:cmds res.left path)
                    in
                    let str_diff =
                        str_diff ^ (added_lines ~cmds:cmds res.right path)
                    in
                    Diff_string { res with ppath = ppath_l; udiff = str_diff; }
            | _, _ -> let ov_set = ValueS.of_list ov in
                      let v_set = ValueS.of_list v in
                      let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                      let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                      let sub_tree =
                          if not (is_empty sub_vals) then
                              clone ~set_values:(Some sub_vals) res.left res.sub path
                          else
                              res.sub
                      in
                      let add_tree =
                          if not (is_empty add_vals) then
                              clone ~set_values:(Some add_vals) res.right res.add path
                          else
                              res.add
                      in
                      let str_diff =
                          str_diff ^ (removed_lines ~cmds:cmds sub_tree path)
                      in
                      let str_diff =
                          str_diff ^ (added_lines ~cmds:cmds add_tree path)
                      in
                      Diff_string { res with ppath = ppath_l; udiff = str_diff;
                                             sub = sub_tree; add = add_tree; }

let add_empty_path src_node dest_node path =
    clone ~recurse:false ~set_values:(Some []) src_node dest_node path

let compare_at_path_maybe_empty left right path =
    let left =
        try
            tree_at_path path left
        with (Empty_comparison path) ->
            try
                let left = add_empty_path right left path in
                tree_at_path path left
             with Vytree.Nonexistent_path ->
                 let a = ["compare_at_path_maybe"] @ path in raise (Empty_comparison a)
     and right =
        try
            tree_at_path path right
        with (Empty_comparison path) ->
            try
                let right = add_empty_path left right path in
                tree_at_path path right
             with Vytree.Nonexistent_path ->
                 let a = ["compare_at_path_maybe"] @ path in raise (Empty_comparison a)
    in (left, right)

let show_diff ?(cmds=false) path left right =
    if (name_of left) <> (name_of right) then
        raise Incommensurable
    else
        let (left, right) =
            if (path <> []) then
                compare_at_path_maybe_empty left right path
            else (left, right) in
        let trees = make_diff_trees left right in
        let udiff = ref "" in
        ppath := [""];
        diff [] (unified_diff ~cmds:cmds udiff trees) [(Option.some left, Option.some right)];
        if cmds then order_commands udiff;
        !udiff

let show_diff_immut ?(cmds=false) path left right =
    if (name_of left) <> (name_of right) then
        raise Incommensurable
    else
        let (left, right) =
            if (path <> []) then
                compare_at_path_maybe_empty left right path
            else (left, right) in
        let dstr = make_diff_string left right in
        let dstr =
            diff_immut [] dstr (unified_diff_immut ~cmds:cmds) [(Option.some left, Option.some right)]
        in
        let dstr = eval_result dstr in
        let strs =
            if cmds then order_commands_immut dstr.udiff
            else dstr.udiff
        in
        strs

let union_of_values (n : Config_tree.t) (m : Config_tree.t) =
    let set_n = ValueS.of_list (data_of n).values in
    let set_m = ValueS.of_list (data_of m).values in
    ValueS.elements (ValueS.union set_n set_m)

let union_of_children n m =
    let set_n = ChildrenS.of_list (children_of n) in
    let set_m = ChildrenS.of_list (children_of m) in
    ChildrenS.elements (ChildrenS.union set_n set_m)

(* tree_union is currently used only for unit tests, so only values of data
   are considered. Should there be a reason to expose it in the future,
   consistency check and union of remaining data will need to be added.
 *)
let rec tree_union s t =
    let child_of_union s t c =
        let s_c = Vytree.find s (name_of c) in
        let t_c = Vytree.find t (name_of c) in
        match s_c, t_c with
        | Some child, None -> clone s t [(name_of child)]
        | None, Some _ -> t
        | Some u, Some v ->
                if u ^~ v then
                    let values = union_of_values u v in
                    let data = {(data_of v) with Config_tree.values = values} in
                    Vytree.replace t (Vytree.make data (name_of v))
                else
                    Vytree.replace t (tree_union u v)
        | None, None -> raise Nonexistent_child
    in
    List.fold_left (fun x c -> child_of_union s x c) t (union_of_children s t)
