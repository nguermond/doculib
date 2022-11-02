
module Tag : sig
  type t = string
end
  
val add_tags : Tag.t list -> unit

val add_synonyms : ((Tag.t * Tag.t) list) -> unit
      
val add_subtags : ((Tag.t * Tag.t) list) -> unit

val compute_trans_closure : unit -> unit

val get_subtags : (Tag.t list) -> Tag.t list
