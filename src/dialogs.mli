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

exception InitializationError
    
val choose_dir : string -> string option

val error_dialog : string -> unit

val edit_notes_dialog : doc:Doc.t -> string
  
val help_dialog : unit -> unit
  
val about_dialog : unit -> unit  
  
val new_library : notebook:Notebook.notebook -> (string * Metadb.Path.root) option

val new_abstract_library : notebook:Notebook.notebook -> unit
  
val manage_libraries : notebook:Notebook.notebook -> unit

val manage_tags : notebook:Notebook.notebook -> unit
    
val search_metadata : default:Doc.t -> doc_type:string
                      -> search_str:string -> Doc.t option
