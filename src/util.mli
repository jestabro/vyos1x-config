exception Syntax_error of ((int * int) option * string)

val get_lexing_position : Lexing.lexbuf -> int * int

val escape_string : string -> string

val default : 'a -> 'a option -> 'a

val numeric_lex_compare : string -> string -> bool
