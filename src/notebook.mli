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

exception LibraryDoesNotExist of string
exception ModelNotLoaded of string
exception NoLibrary

open Metadb
        
class library : string -> string -> GPack.box -> GMisc.label ->
  object      
    method get_name : string
    method get_doc_type : string
    method get_page : GPack.box
    method get_label : GMisc.label
    method get_model : Model.t
    method rename : string -> unit
    method is_loaded : bool
    method set_model : Model.t -> unit
  end
                               
class notebook : GPack.notebook -> GMenu.menu -> (string -> bool)
  -> object
  val mutable libraries : (string * library) list
                                                 
  method init : ((string * string) list) -> unit
  method add_library : library:string -> doc_type:string -> prepend:bool -> unit
  method remove_library : delete_metadata:bool -> library:string -> unit
  method rename_library : library:string -> string -> bool
       
  method load_library : library:string -> unit
  method refresh_library : library:string -> unit

  (* TODO: should not be public *)
  method current_library : string * library
           
  method refilter : ?library:(string option) -> unit -> unit
  method action_on_selected : action:(Model.t -> string -> Model.key -> Path.rel -> unit)
                              -> unit
  method edit_selected : editor:(Path.rel -> Doc.t -> Doc.t option) -> unit
end
