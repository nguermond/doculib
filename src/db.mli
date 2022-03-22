
exception LibraryExists
exception InvalidLibrary
        
val current_branch : string
val configdir : string
val datadir : string
val libconfig : string
    
val get_rel_path : library:string -> string -> string
val get_files : library:string -> string -> string list

val set_lib_version : string -> unit
val init_lib_config : unit -> unit
val get_lib_version : unit -> string


type doc = Doc.t
                                                                           
class db : string ->
object
  val store : string
  val mutable libraries : ((string * (string * string)) list)

  method get_document : library:string -> path:string -> doc
  method get_documents : library:string -> doc list
    
  method set_document : library:string -> path:string -> doc -> unit

  method add_document : library:string -> doc -> unit

  method remove_document : library:string -> path:string -> unit


  method get_libraries : unit -> string list
  method get_library_doc_type : library:string -> string
  method add_library : library:string -> root:string -> doc_type:string -> unit
  method remove_library : library:string -> unit
       
  method get_library_root : library:string -> string
  method import_files : library:string -> doc_type:string -> (string list) ->  doc list


  method get_full_path : library:string -> string -> string

  method open_doc : library:string -> path:string -> unit
  method check_library_integrity : library:string -> doc list
end
