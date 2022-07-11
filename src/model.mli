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
  val missing : bool GTree.column
  val duplicate : bool GTree.column
    
  val get_name : int -> string
end

type t
type key

module Entry :
sig
  type t
  val make : Path.rel -> ?missing:bool -> ?duplicate:bool -> Doc.t -> t
  val get_doc : t -> Doc.t
  val is_missing : t -> bool
  val is_duplicate : t -> bool
  val get_path : t -> Path.rel
end
     
val remove_entry : t -> key:key -> unit

val set_entry : t -> key:key -> Entry.t -> unit

val add_entry : t -> Entry.t -> unit

val get_entry : t -> key:key -> Entry.t

val is_missing : t -> key:key -> bool

val is_duplicate : t -> key:key -> bool
  
val flag_missing : t -> key:key -> bool -> unit

val flag_duplicate : t -> key:key -> bool -> unit

val set_message : t -> key:key -> string -> unit
  
val import_documents : t -> (Path.rel * Doc.t) list -> unit
  
val reset_model : t -> unit
  
val reset_sort_indicators : t -> unit
  
(* Handle click events *)
val handle_click_events : t -> context_menu:GMenu.menu -> unit

val refilter : t -> unit

val set_visible_func : t -> (string -> bool) -> unit

val iter_selected : t -> action:(key -> Path.rel -> unit) -> unit

val on_selected : t -> (key -> Path.rel -> 'a) -> 'a
  
val iter : t -> action:(key -> Path.rel -> 'a -> unit) -> (Path.rel * 'a) list -> unit
  
val make_document_list : ?height:int -> library:string -> doc_type:string ->
                         ?sort:(('a GTree.column) option) ->
                         packing:(GObj.widget -> unit) -> (Path.rel * Doc.t) list -> t

                                                                                                                      
val make_entry_list : ?height:int -> doc_type:string ->
                      ?sort:(('a GTree.column) option) ->
                      packing:(GObj.widget -> unit) -> (Path.rel * Doc.t) list -> t
