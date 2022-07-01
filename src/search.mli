exception SearchFailure of string

val search_document : string -> string -> string -> Doc.t list
val get_database_name : string -> string
