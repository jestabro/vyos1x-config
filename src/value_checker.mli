type value_constraint =
    | Regex of string [@name "Regex"]
    | External of string * string option [@name "Exec"]
    [@@deriving yojson]

exception Bad_validator of string

val validate_value : string -> value_constraint -> string -> bool

val validate_any : string -> value_constraint list -> string -> bool
