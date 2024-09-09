module type T =
    sig
        type t
        val to_yojson : t -> Yojson.Safe.t
        val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
        val default : t
    end

module type FI = functor (M: T) ->
    sig
        val write_internal : M.t -> string -> unit
        val read_internal : string -> M.t
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
end
