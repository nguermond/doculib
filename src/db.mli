
val current_branch : string
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
             

  

(* val add_document : library:string -> doc -> unit *)
  

val get_rel_path : library:string -> string -> string




  
val pp_doc : Format.formatter -> doc -> unit
val edit_document : attribute -> doc -> doc




                                                                                 
class db : string ->
object
  val store : string
  val mutable libraries : ((string * (string * string)) list)

  method get_document : library:string -> path:string -> doc
  method get_documents : library:string -> doc list
    
  method set_document : library:string -> path:string -> doc -> unit

  method remove_document : library:string -> path:string -> unit


  method get_libraries : unit -> string list
  method get_library_doc_type : library:string -> string
  method add_library : library:string -> root:string -> doc_type:string -> unit
  method get_library_root : library:string -> string
  method import_files : library:string -> doc_type:string -> (string list) ->  doc list


  method get_full_path : library:string -> string -> string

  method open_doc : library:string -> path:string -> unit 
end
