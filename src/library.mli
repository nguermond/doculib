exception InvalidDocType


type doc_type = [`Book | `Document]

(* TODO : these should be private *)
val string_of_doc_type : doc_type -> string
val doc_type_of_string : string -> doc_type
              
type t = {
    version : string;
    doc_type : doc_type;
  }

val get_doc_type : t -> doc_type

val get_version : t -> string

val to_json : t -> Json.t
  
val from_json : Json.t -> t
  
val make : doc_type -> t
