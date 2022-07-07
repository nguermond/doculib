(** [Metadb] is a library to locally store and manipulate a JSON database of file metadata. 
    A key feature of [Metadb] is that the user may rename or move files within a library, or move files between directories,
    and [Metadb] will resolve missing or renamed files according to their MD5 hash. [Metadb] can also find duplicate files across libraries.

    A database is composed of one or more libraries, each of which points to a specific directory in the [$HOME] path.
    A library may have metadata specified by {!LibData}, such as its version of the type of its entries. 
    Name, path, and metadata of each library is stored as JSON in a configuration file which may be read and written by {!Make.load_config} and {!Make.write_config}. 
    For example, such a file may be stored in [$HOME/.config/libraries.json].

    Each library assigns a JSON file to each file in its associated directory, 
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

    The key to an entry or file in a library is always given by its {{!Path.rel} relative path} with respect to the library's {{!Make.get_library_root} root path}.
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

  (** For debugging purposes *)
  val to_string : t -> string
end

(** Metadata associated to each library *)
module type LibData =
  sig
    type t
    val to_json : t -> Json.t
    val from_json : Json.t -> t
  end


(** Make a [Metadb] database *)
module Make : functor (D : Metadata) (LD : LibData) ->
  sig
    (** {2 Library configuration} *)
    
    (** Load database configurations from file *)
    val load_config : Path.root -> unit

    (** Write database configuration to file *)  
    val write_config : Path.root -> unit

    (** {2 Managing libraries} *)
      
    (** Create a new library with name [library] pointing to the given path *)
    val new_library : library:string -> Path.root -> LD.t -> unit

    (** Remove the given library. If [delete_metadata] is true, 
        the [.metadata] directory will be deleted as well. *)
    val remove_library : delete_metadata:bool -> library:string -> unit

    (** Rename the given library *)
    val rename_library : library:string -> string -> unit

    (** Move library to a new path and migrate all files and entries.
        Raises {!DirNotEmpty} if new directory is not empty *)
    val move_library : library:string -> Path.root -> unit


    (** {2 Initializing and refreshing libraries} *)
      
    (** Initialize a specific library. This loads existing entries and adds a new entry for every new file without entry *)
    val init_library : library:string -> unit

    (** Initialize all libraries *)
    val init_libraries : unit -> unit

    (** Refresh a specific library. This adds a new entry for every new file without entry *)
    val refresh_library : library:string -> (Path.rel * D.t) Seq.t

    (** {2 Reading and modifying data} *)
      
    (** Get all libraries and their associated metadata *)
    val get_libdata : unit -> (string * LD.t) list

    (** Returns root path of specified library *)
    val get_library_root : library:string -> Path.root

    (** Get all entries of a specified library and their associated metadata *)
    val get_entries : library:string -> (Path.rel * D.t) Seq.t

    (** If it exists, get the value of an entry in the specified library. 
        The key to an entry is the relative path of the file with respect to the library's {{!get_library_root} root} *)
    val get_entry : library:string -> Path.rel -> D.t option

    (** Set the value of an entry in the specified library. Raises {!EntryDoesNotExist} if key does not exist *)
    val set_entry : library:string -> Path.rel -> D.t -> unit

    (** Remove an entry from a specified library, but leave the file *)
    val remove_entry : library:string -> Path.rel -> unit

    (** Remove a file from a specified library, but leave the entry *)
    val remove_file : library:string -> Path.rel -> unit
      
    (** Move entry and file from one library to another. 
        Raises
        - {!EntryExists} if entry already exists in [to_lib]
        - {!FileExists} if file already exists in [to_lib]
        - {!EntryDoesNotExist} if entry does not exist in [from_lib] *)
    val migrate_entry : from_lib:string -> to_lib:string -> Path.rel -> unit
      
    (** {2 Missing and duplicate files} *)
      
    (** Needed in order to quickly {{!resolve_missing_files} resolve missing files} or {{!find_duplicates} find duplicates}. 
        This should only be called after freshly initializing or refreshing a library *)
    val index_files : unit -> unit

      (** Return type of {!resolve_missing_files}. 
          - [Remap (key, (library',key'))] signifies that entry [key] has been
            moved to [library'] with new key [key']
          - [Missing key] signifies that no file could be associated with entry [key] *)
    type resolution = Remap of (Path.rel * (string * Path.rel))
                    | Missing of Path.rel
      
    (** Resolve moved or renamed files. Entries may no longer point to a file if the user moves or renames files.
        For each entry no longer pointing to a file, this function searches accross all libraries for a file with matching hash.
        This function assumes 
        - All libraries are {{!init_libraries} freshly initialized} or have been {{!refresh_library} refreshed}
        - Files have been {{!index_files} indexed}
        Note: this function cannot resolve duplicate conflicts *)
    val resolve_missing_files : library:string -> resolution Seq.t

    (** Return a list of entries in a library for which there exists another entry (across all libraries) pointing to a file of the same hash.
        This function assumes 
        - All libraries are {{!init_libraries} freshly initialized} or have been {{!refresh_library} refreshed}
        - Files have been {{!index_files} indexed} *)
    val find_duplicates : library:string -> (Path.rel) Seq.t
      
    (** {2 Writing data to disk} *)
      
    (** Write modified metadata for specified library to disk *)
    val flush_library_metadata : library:string -> unit
      
    (** Flush metadata for all libraries *)
    val flush_metadata : unit -> unit

    (** {2 Debugging} *)

    (** Flush entire library to a string, for debugging purposes *)
    val library_to_string : library:string -> string
  end

