
type t

val hash_file : Path.root -> t

val to_string : t -> string
val of_string : string -> t

val equal : t -> t -> bool

val to_int : t -> int
