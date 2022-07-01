
exception IncorrectPathType of string

type root
type rel
type name
type path = name list
       
val mk_root : string -> root
val mk_rel : string -> rel
val mk_name : string -> name
val mk_path : string list -> path

val to_string : root -> string
val rel_to_string : rel -> string
val string_of_name : name -> string
  
val merge_lst : root -> path -> root
val merge : root -> rel -> root

val split : rel -> path
val unroot : root -> root * rel
  
val remove_file_ext_rel : string -> rel -> rel
val remove_file_ext : string -> root -> root

(* Drop a root from a path to make it relative *)
val strip_root : root -> root -> rel

(* Drop leaf from path *)
val drop_leaf : root -> root

(* Get leaf, ie. file name *)
val get_leaf : rel -> name
