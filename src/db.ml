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
  Log.push "Loading library configuration file";
  Libraries.load_config libconfig;
  Log.push "Initializing libraries";
  Libraries.init_libraries ()

let flush_data () =
  Log.push "Flushing libconfig";
  Libraries.write_config libconfig;
  Libraries.flush_metadata()

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

let remove_library ~delete_metadata ~library : unit =
  Libraries.remove_library ~delete_metadata ~library

let rename_library ~library name : unit =
  Libraries.rename_library ~library name
  
let get_documents ~library : (Path.rel * Doc.t) list =
  List.of_seq (Libraries.get_entries ~library)

(* returns new entries *)
let refresh_library ~library : (Path.rel * Doc.t) list =
  List.of_seq (Libraries.refresh_library ~library)

(* Assumes library was just refreshed *)
let resolve_missing_files ~library : Path.rel list =
  Libraries.index_files();
  Seq.filter_map (function
      | Libraries.Missing key ->
         prerr_endline ("Missing file: "^(Path.string_of_rel key));
         Some key
      | Libraries.Remap (key,(library',key')) ->
         prerr_endline ("Remapped file: "^(Path.string_of_rel key)
                        ^"\n -> "^library'^" : "^(Path.string_of_rel key'));
         None)
    (Libraries.resolve_missing_files ~library)
  |> List.of_seq


(* Assumes library was just refreshed *)
let find_duplicates () : (string * Path.rel) list =
  Libraries.index_files();
  Libraries.find_duplicates ()
