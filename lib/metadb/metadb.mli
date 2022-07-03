(** [Metadb] is a library to locally store and manipulate a json database of file metadata. 
    A key feature of [Metadb] is that the user may rename or move files within a library, or move files between directories,
    and [Metadb] will find and resolve missing or renamed files according to its MD5 hash.  

    A database is composed of one or more libraries, each of which points to a specific directory in the [$HOME] path.
    A library may have metadata specified by {!LibData}, such as its version of the type of its entries. 
    Information about each library is stored in a configuration file which must be read and written by {!Make.load_config} and {!Make.write_config}.

    Each library assigns a [json] file to each file in its associated directory, 
    for example, consider a library named "documents" that points to a directory [~/Documents/Articles]
    and suppose the following files are contained in this library:

    [~/Documents/Articles/]
    - [article1.pdf]
    - [article2.djvu]
    - [path/to/article3.pdf]

    Then [Metadb] will store metadata for each file specified by {!Metadata} in a hidden directory [./.metadata]:
    [~/Documents/Articles/.metadata]
    - [article1.pdf.json]
    - [article2.djvu.json]
    - [path/to/article3.pdf.json]
 *)


exception FileExists of Path.root
exception EntryExists of (string * Path.rel)
exception EntryDoesNotExist of (string * Path.rel)
exception DirNotEmpty of Path.root 
exception LibraryExists


module Path : module type of Path
module Hash : module type of Hash
module Json : module type of Json
module System : module type of System

(** Metadata to be associated to each file in a library *) 
module type Metadata =
sig
  type t
  val to_json : t -> Json.t
  val from_json : Json.t -> t

  (** An constant value must be specified for initialization *)
  val init : t

  (** Only for debugging purposes *)
  val to_string : t -> string
end

(** Metadata associated to each library *)
module type LibData =
  sig
    type t
    val to_json : t -> Json.t
    val from_json : Json.t -> t
  end

module Make : functor (D : Metadata) (LD : LibData) ->
  sig
    val init_library : library:string -> unit             
    val refresh_library : library:string -> (Path.rel * D.t) Seq.t
    
    val init_libraries : unit -> unit

    
    val add_entry : library:string -> Path.rel -> D.t -> unit
    val get_entry : library:string -> Path.rel -> D.t option
    val set_entry : library:string ->  Path.rel -> D.t -> unit
    val remove_entry : library:string ->  Path.rel -> unit
    val remove_file : library:string -> Path.rel -> unit
      
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

    val get_entries : library:string -> (Path.rel * D.t) Seq.t

    val get_libdata : unit -> (string * LD.t) list
    val get_library_root : library:string -> Path.root

    val flush_metadata : unit -> unit
  end

