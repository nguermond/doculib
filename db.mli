val configdir : string

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
             
val import_files : library:string -> doc_type:string -> (string list) ->  doc list

  
val get_documents : library:string -> doc list
val get_document : library:string -> path:string -> doc
val set_document : library:string -> path:string -> doc -> unit
val remove_document : library:string -> path:string -> unit

val edit_document : attribute -> doc -> doc

val get_rel_path : library:string -> string -> string
val get_full_path : library:string -> string -> string
val get_library_root : library:string -> string
val get_library_doc_type : library:string -> string
val get_libraries : unit -> string list

  
val pp_doc : Format.formatter -> doc -> unit

val add_library : library:string -> root:string -> doc_type:string -> unit

val open_doc : library:string -> path:string -> unit
