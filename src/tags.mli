
module Tag : sig
  type t = string
end


type t
   
type pair = Tag.t * Tag.t


val init : unit -> t
  
val add_tags : t -> Tag.t list -> unit

val add_synonyms : t -> (pair list) -> unit
  
val add_subtags : t -> (pair list) -> unit

val compute_trans_closure : t -> unit

val get_subtags : t -> (Tag.t list) -> Tag.t list

val load_from_file : Metadb.Path.root -> t

val get_synonyms : t -> pair list
val get_subtags : t -> pair list
