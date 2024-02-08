external handle_init: unit -> int = "handle_init"
external handle_free: int -> unit = "handle_free"
external in_config_session_handle: int -> bool = "in_config_session_handle"
external in_config_session: unit -> bool = "in_config_session"
external set_path: int -> string array -> int -> int = "set_path" [@@noalloc]
external delete_path: int -> string array -> int -> int = "delete_path" [@@noalloc]
external set_path_reversed: int -> string array -> int -> int = "set_path_reversed" [@@noalloc]
external delete_path_reversed: int -> string array -> int -> int = "delete_path_reversed" [@@noalloc]
