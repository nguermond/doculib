

type doc = {star : bool;
            title : string;
            authors : string list;
            doi : string;
            isbn : string;
            year : string;
            tags : string list;
            path : string;
            doc_type : string;
           }

type attribute =
  Star of bool
| Title of string
| Authors of string list
| Doi of string
| Isbn of string
| Year of string
| Tags of string list
| Path of string
| DocType of string

val init : unit -> unit
  
val set_attribute : string -> string -> attribute
             
module Doc : Irmin.Contents.S with type t = doc

val import_file : library:string -> path:string -> doc_type:string -> doc option

val get_documents : library:string -> doc list
val get_document : library:string -> path:string -> doc
val set_document : library:string -> path:string -> doc:doc -> unit
val remove_document : library:string -> path:string -> unit

val edit_document : attribute -> doc -> doc


val get_library_root : library:string -> string
val get_library_doc_type : library:string -> string
val get_libraries : unit -> string list

  
val pp_doc : Format.formatter -> doc -> unit

val add_library : library:string -> root:string -> doc_type:string -> unit
