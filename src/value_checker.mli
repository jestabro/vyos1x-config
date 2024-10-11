(*type value_constraint = Regex of string | External of string * string option*)

type value_constraint =
    | Regex of string [@name "regex"]
    | External of string * string option [@name "exec"]
    [@@deriving yojson]

exception Bad_validator of string

val validate_value : string -> Buffer.t -> value_constraint -> string -> bool

val validate_any : string -> value_constraint list -> string -> bool * string

val validate_all : string -> value_constraint list -> string -> bool * string
