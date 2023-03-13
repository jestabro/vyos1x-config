exception Syntax_error of ((int * int) option * string)

external node_name_compare : string -> string -> int = "caml_node_name_compare"

val get_lexing_position : Lexing.lexbuf -> int * int

val escape_string : string -> string

val default : 'a -> 'a option -> 'a
