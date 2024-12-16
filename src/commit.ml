module CT = Config_tree
module CD = Config_diff
module RT = Reference_tree

type commit_data = {
    script: string option;
    priority: int;
    tag_value: string option;
    arg_value: string option;
    path: string list;
} [@@deriving show]


let default_commit_data = {
    script = None;
    priority = 0;
    tag_value = None;
    arg_value = None;
    path = [];
}

let lex_order c1 c2 =
    match c1.tag_value, c2.tag_value with
    | Some t1, Some t2 -> Util.lexical_numeric_compare t1 t2
    | _ ->
        match (Util.get_last c1.path), (Util.get_last c2.path) with
        | Some p1, Some p2 ->
            compare p1 p2
        | _ ->
            match c1.script, c2.script with
            | Some s1, Some s2 ->
                compare (FilePath.basename s1) (FilePath.basename s2)
            | _ -> 0

module CI = struct
    type t = commit_data
    let compare a b =
        match compare a.priority b.priority with
        | 0 -> lex_order a b
        | c -> c
end
module CS = Set.Make(CI)

let owner_args_from_data p s =
    match s with
    | None -> None, None
    | Some o ->
    let oa = Pcre.split o in
    let owner = List.nth oa 0 in
    if List.length oa < 2 then Some owner, None
    else
    let var = List.nth oa 1 in
    let res = Pcre.extract_all ~pat:"\\.\\./" var in
    let var_pos = Array.length res in
    let arg_value = Util.get_last_n p var_pos
    in Some owner, arg_value
(*
type node_acc = string list * CS.t
*)
let add_tag_instance cd cs tv =
    CS.add { cd with tag_value = Some tv; } cs

let get_commit_data rt ct (path, cs') t =
    if Util.is_empty path then
        (path, cs')
    else
    if (Vytree.name_of_node t) = "" then
        (path, cs')
    else
    let rpath = List.rev path in
    (* the following is critical to avoid redundant calculations for owner
       of a tag node, quadratic in the number of tag node values *)
    if CT.is_tag_value ct rpath then
        (path, cs')
    else
    let rt_path = RT.refpath rt rpath in
    let priority =
        match RT.get_priority rt rt_path with
        | None -> 0
        | Some s -> int_of_string s
    in
    let owner = RT.get_owner rt rt_path in
    if  owner = None then (path, cs')
    else
    let (own, arg) = owner_args_from_data rpath owner in
    let c_data = { default_commit_data with
                   script = own;
                   priority = priority;
                   arg_value = arg;
                   path = path; }
    in
    let tag_values =
        match RT.is_tag rt rt_path with
        | false -> []
        | true -> Vytree.list_children t
    in
    let cs =
        match tag_values with
        | [] -> CS.add c_data cs'
        | _ -> List.fold_left (add_tag_instance c_data) cs' tag_values
    in (path, cs)

let test_commit_data rt ct =
    let cs =
        Vytree.fold_tree_with_path (get_commit_data rt ct) ([],CS.empty) ct
    in
    let sprint_commit_data acc s =
        acc ^ "\n" ^ show_commit_data s
    in
    let out = List.fold_left sprint_commit_data "" (CS.elements (snd cs))
    in print_endline out
