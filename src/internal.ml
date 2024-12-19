exception Incompatible_union
exception Nonexistent_child

module type T =
    sig
        type t
        val to_yojson : t -> Yojson.Safe.t
        val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
        val default : t
        val compare : t -> t -> int
        val name_of : t -> string
        val data_of : t -> 'a
        val children_of : t -> t list
        val insert_child : t -> t -> t
        val replace_child : t -> t -> t
        val replace_data : t -> 'a -> t
    end

module type FI = functor (M: T) ->
    sig
        val write_internal : M.t -> string -> unit
        val read_internal : string -> M.t
        val tree_union : (M.t -> M.t -> 'a) -> M.t -> M.t -> M.t
    end

module Make : FI = functor (M: T) -> struct
    let write_internal x file_name =
        let yt = M.to_yojson x in
        let ys = Yojson.Safe.to_string yt in
        let oc = open_out file_name in
        Printf.fprintf oc "%s" ys; close_out oc

    let read_internal file_name =
        let ic = open_in file_name in
        let ys = really_input_string ic (in_channel_length ic) in
        let yt = Yojson.Safe.from_string ys in
        let ct_res = M.of_yojson yt in
        let ct = Result.value ct_res ~default:M.default in
        close_in ic; ct

    module ChildrenS = Set.Make(struct
        type t = M.t
        let compare = M.compare
    end)

    let union_of_children n m =
        let set_n = ChildrenS.of_list (M.children_of n) in
        let set_m = ChildrenS.of_list (M.children_of m) in
        ChildrenS.elements (ChildrenS.union set_n set_m)

    let rec tree_union f s t =
        if (M.name_of s) <> (M.name_of t) then
            raise Incompatible_union
        else
        let child_of_union s t c =
(*            let s_c = Vytree.find s (M.name_of c) in
            let t_c = Vytree.find t (M.name_of c) in*)
            let s_c = M.find_child s c in
            let t_c = M.find_child t c in
            match s_c, t_c with
            | Some c, None ->
                insert_child t c
(*                Vytree.insert ~position:Lexical ~children:(M.children_of c) t [(M.name_of c)] (M.data_of c)*)
            | None, Some _ -> t
            | Some u, Some v ->
                    if M.data_of u <> M.data_of v then
                        let data = f u v in
                        M.replace_data t data
                        Vytree.replace t (Vytree.make data (M.name_of v))
                    else
                        M.replace_child t (tree_union u v)
(*                        Vytree.replace t (tree_union u v)*)
            | None, None -> raise Nonexistent_child
        in
        List.fold_left (fun x c -> child_of_union s x c) t (union_of_children s t)
end
