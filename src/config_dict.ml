(** The goal here is to produce a JSON rendering to load into a Python dict.
 *)

(*let config_dict ?(mangle=false) ?(no_tag_mangle=false)*)

let config_dict _rt ct =
    Config_tree.render_json ct
