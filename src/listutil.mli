exception InternalError

val index : 'a list -> 'a -> int option

val assoc_index : ('a * 'b) list -> 'a -> int option
  
val insert : ('a list) -> int -> 'a -> 'a list

val rename_assoc : 'a -> 'a -> ('a * 'b) list -> ('a * 'b) list
