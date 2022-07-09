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
open Metadb

type cell_renderer
type column
type packing
type row = Gtk.tree_iter

val dnd_targets : Gtk.target_entry list
  
module Attr :
  sig
    val star : bool GTree.column
    val authors : string GTree.column
    val title : string GTree.column
    val year : string GTree.column
    val doi : string GTree.column
    val isbn : string GTree.column
    val tags : string GTree.column
    val path : string GTree.column
      
    val get_name : int -> string
  end

class model : GTree.model_filter -> GTree.list_store -> GTree.view ->
  object
    method get_row : Gtk.tree_path -> row
         
    method get : 'a. row:row -> column:('a GTree.column) -> 'a

    method remove : row:row -> unit

    method remove_entry_from_path : path:Path.rel -> unit
      
    method set_entry : row:row -> Path.rel -> Doc.t -> unit
      
    method import_documents : (Path.rel * Doc.t) list -> unit
         
    method reset_model : unit -> unit
         
    method reset_sort_indicators : unit -> unit
         
    method add_column : title:string -> width:int ->
                        cell_renderer:cell_renderer -> column -> unit

    (* Handle click events *)
    method handle_click_events : context_menu:GMenu.menu -> unit

    method refilter : unit -> unit
    method set_visible_func : (GTree.model -> row -> bool) -> unit
    method get_selected_rows : Gtk.tree_path list
  end

    
val make_document_list : ?height:int -> ?library:string -> doc_type:string ->
                         ?sort:(('a GTree.column) option) ->
                         packing:(GObj.widget -> unit) -> (Path.rel * Doc.t) list -> model

                                                                                                                      
val make_entry_list : ?height:int -> doc_type:string ->
                      ?sort:(('a GTree.column) option) ->
                      packing:(GObj.widget -> unit) -> (Path.rel * Doc.t) list -> model
