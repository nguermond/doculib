
exception EnvVarNotSet
       
exception LibraryExists
   
exception CannotMigrate


val init : unit -> unit
  
val add_library : library:string -> root:Path.root -> Library.t -> unit
val remove_library : library:string -> unit
val rename_library : library:string -> string -> unit


val get_library_root : library:string -> Path.root

val get_file : library:string -> path:Path.rel -> Path.root
val get : library:string -> path:Path.rel -> Doc.t
val set : library:string -> path:Path.rel -> Doc.t -> unit
val remove_entry : library:string -> path:Path.rel -> unit
val remove_file : library:string -> path:Path.rel -> unit
val migrate : from_lib:string -> to_lib:string -> path:Path.rel -> unit


val get_documents : library:string -> (Path.rel * Doc.t) list
val get_library_descriptions : unit -> (string * Library.t) list

val refresh_library : library:string -> (Path.rel * Doc.t) list

val resolve_missing_files : library:string -> unit

val flush_data : unit -> unit
