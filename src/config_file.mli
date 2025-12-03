
val escape_backslash : string -> string

val unescape_backslash : string -> string

val load_config : string -> (Config_tree.t, string) result

val save_config : Config_tree.t -> string -> (unit, string) result
