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
       
exception LibraryExists
   
exception CannotMigrate

exception InitializationError of string

open Metadb

val init : unit -> unit
  
val add_library : library:string -> root:Path.root -> Library.t -> unit
val remove_library : delete_metadata:bool -> library:string -> unit
val rename_library : library:string -> string -> unit


val get_library_root : library:string -> Path.root
val get_doc_type : library:string -> Library.doc_type

val get_file : library:string -> path:Path.rel -> Path.root
val get : library:string -> path:Path.rel -> Doc.t
val set : library:string -> path:Path.rel -> Doc.t -> unit
val remove_entry : library:string -> path:Path.rel -> unit
val remove_file : library:string -> path:Path.rel -> unit
val migrate : from_lib:string -> to_lib:string -> path:Path.rel -> unit


val get_documents : library:string -> (Path.rel * Doc.t) list
val get_library_descriptions : unit -> (string * Library.t) list

val refresh_library : library:string -> (Path.rel * Doc.t) list

val resolve_missing_files : library:string -> (Path.rel * bool) list

val find_duplicates : unit -> ((string * Path.rel) list) list
  
val flush_metadata : unit -> unit
val flush_libconfig : ?ord:(string list) -> unit -> unit
