
exception FileExists of Path.root
exception EntryExists of (string * Path.rel)
exception EntryDoesNotExist of (string * Path.rel)

exception DirNotEmpty of Path.root 


module type Metadata =
sig
  type t
  val to_json : t -> Json.t
  val from_json : Json.t -> t
  val init : t
  val to_string : t -> string
end
                
module type LibData =
  sig
    type t
    val to_json : t -> Json.t
    val from_json : Json.t -> t
  end

module Make : functor (D : Metadata) (LD : LibData) ->
  sig
             
    val refresh_library : library:string -> unit
    val init_library : library:string -> unit
    
    val refresh_libraries : unit -> unit
    val init_libraries : unit -> unit
    
    val add_entry : library:string -> Path.rel -> D.t -> unit
    val get_entry : library:string -> Path.rel -> D.t option
    val set_entry : library:string ->  Path.rel -> D.t -> unit
    val remove_entry : library:string ->  Path.rel -> unit
    
    (* Move entry and file from one library to another *)
    val migrate_entry : from_lib:string -> to_lib:string -> Path.rel -> unit
    (* Move and rename entry from one library to another *)
    val remap_entry : from_lib:string -> to_lib:string
                      -> Path.rel -> Path.rel -> unit
    
    val new_library : library:string -> Path.root -> LD.t -> unit
    val remove_library : library:string -> unit
    val rename_library : library:string -> string -> unit
    (* move library to a new path and migrate 
     * all files and entries *)
    val move_library : library:string -> Path.root -> unit
    
    val index_files : unit -> unit
    
    (* This function assumes 
     * 1. libraries are freshly initialized or have been refreshed
     * 2. files have been indexed *)
    val resolve_missing_files : library:string -> unit

    val load_config : Path.root -> unit
    val write_config : Path.root ->  unit
  end

