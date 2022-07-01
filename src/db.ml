exception EnvVarNotSet

open Metadb

exception LibraryExists
exception EntryDoesNotExist of string * Path.rel
exception CannotMigrate          

                   
let configdir : Path.root =
  match (Sys.getenv_opt "XDG_CONFIG_HOME") with
  | Some usr_config -> Path.merge_lst (Path.mk_root usr_config)
                         [(Path.mk_name "doculib")]
  | None -> 
     (match (Sys.getenv_opt "HOME") with
      | Some home -> Path.merge (Path.mk_root home)
                       (Path.mk_rel ".config/doculib")
      | None -> raise EnvVarNotSet)

let libconfig : Path.root =
  Path.merge_lst configdir [(Path.mk_name "libraries.json")]


       
module Libraries = Make(Doc)(Library)


let add_library ~library ~root (libdata : Library.t) : unit =
  try
    Libraries.new_library ~library root libdata
  with
  | Metadb.LibraryExists -> raise LibraryExists

let get_library_descriptions () : (string * Library.t) list =
  Libraries.get_libdata ()

let get_library_root ~library : Path.root =
  Libraries.get_library_root ~library

let get_file ~library ~path : Path.root =
  let root = get_library_root ~library in
  Path.merge root path

let remove_entry ~library ~path : unit =
  Libraries.remove_entry ~library path

let remove_file ~library ~path : unit =
  Libraries.remove_file ~library path
  
let get ~library ~path : Doc.t =
  match Libraries.get_entry ~library path with
  | Some doc -> doc
  | None -> raise (EntryDoesNotExist(library,path))

let set ~library ~path doc : unit =
  Libraries.set_entry ~library path doc

let migrate ~from_lib ~to_lib ~path : unit =
  try Libraries.migrate_entry ~from_lib ~to_lib path with
  | FileExists _ | EntryExists _ | EntryDoesNotExist _ ->
     raise CannotMigrate

let remove_library ~library : unit =
  Libraries.remove_library ~library

let rename_library ~library name : unit =
  Libraries.rename_library ~library name
  
let init () =
  Libraries.load_config libconfig;
  Libraries.init_libraries ()

let get_documents ~library : (Path.rel * Doc.t) list =
  List.of_seq (Libraries.get_entries ~library)

(* returns new entries *)
let refresh_library ~library : (Path.rel * Doc.t) list =
  List.of_seq (Libraries.refresh_library ~library)

(* TODO: return list of missing entries *)
let resolve_missing_files ~library : unit =
  Libraries.resolve_missing_files ~library
