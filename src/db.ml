(******************************************************************************)
(* DocuLib                                                                    *)
(* Copyright (C) 2022 Nathan Guermond                                         *)
(*                                                                            *)
(* This program is free software: you can redistribute it and/or modify it    *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation, either version 3 of the License, or (at your option)  *)
(* any later version.                                                         *)
(*                                                                            *)
(* This program is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *)
(* for more details.                                                          *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License along    *)
(* with this program. If not, see <https://www.gnu.org/licenses/>.            *)
(*                                                                            *)
(******************************************************************************)
exception EnvVarNotSet

open Metadb

exception LibraryExists
exception EntryDoesNotExist of string * Path.rel
exception CannotMigrate
exception InitializationError of string

                   
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

let init () =
  Log.push "Checking database compatilibity";
  (try Update_db.init()
   with (Update_db.CannotMigrate msg) ->
     raise (InitializationError msg));
  Log.push "Loading library configuration file";
  Libraries.load_config libconfig;
  Log.push "Initializing libraries";
  Libraries.init_libraries ()

let flush_metadata () =
  Log.push "Flushing metadata";
  Libraries.flush_metadata()
  
let flush_libconfig ?(ord = []) () =
  Log.push "Flushing libconfig";
    Libraries.write_config ~ord libconfig

let add_library ~library ~root (libdata : Library.t) : unit =
  try
    Libraries.new_library ~library root libdata;
    flush_libconfig()
  with
  | Metadb.LibraryExists ->
     raise LibraryExists
                            

let get_library_descriptions () : (string * Library.t) list =
  Libraries.get_libdata ()

let get_library_root ~library : Path.root =
  Libraries.get_library_root ~library

let get_file ~library ~path : Path.root =
  let root = get_library_root ~library in
  Path.merge root path

let get_doc_type ~library : Library.doc_type =
  (List.assoc library (Libraries.get_libdata ())).doc_type

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

let remove_library ~delete_metadata ~library : unit =
  Libraries.remove_library ~delete_metadata ~library;
  flush_libconfig()

let rename_library ~library name : unit =
  Libraries.rename_library ~library name
  
let get_documents ~library : (Path.rel * Doc.t) list =
  List.of_seq (Libraries.get_entries ~library)

(* returns new entries *)
let refresh_library ~library : (Path.rel * Doc.t) list =
  List.of_seq (Libraries.refresh_library ~library)

(* Assumes library was just refreshed *)
(* TODO: Remapped files need to be updated in the notebook *)
let resolve_missing_files ~library : (Path.rel * bool) list =
  Log.push (Format.sprintf "Indexing libraries");
  Libraries.index_files();
  Log.push (Format.sprintf "Resolving missing files for library %s" library);
  let keys =
    Seq.map (function
        | Libraries.Missing key ->
           Log.push (Format.sprintf "Missing file: %s" (Path.string_of_rel key));
           (key,true)
        | Libraries.Remap (key,(library',key')) ->
           Log.push (Format.sprintf "Remapped file: %s\n -> %s : %s"
                       (Path.string_of_rel key) library'
                       (Path.string_of_rel key'));
           (key,false))
      (Libraries.resolve_missing_files ~library)
    |> List.of_seq
  in
  Log.push (Format.sprintf "Flushing metadata for library %s" library);
  Libraries.flush_metadata();
  keys

(* Assumes library was just refreshed & files were indexed *)
let find_duplicates () : ((string * Path.rel) list) list =
  Log.push (Format.sprintf "Finding duplicates");
  Libraries.find_duplicates ()
