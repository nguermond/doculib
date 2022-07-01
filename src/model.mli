

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
         
    method add_column : title:string -> width:int -> ?editable:bool -> ?library:string ->
                        ?cell_renderer:(cell_renderer option) -> column -> unit

    (* Handle click events *)
    method handle_click_events : context_menu:GMenu.menu -> unit

    method refilter : unit -> unit
    method set_visible_func : (GTree.model -> row -> bool) -> unit
    method get_selected_rows : Gtk.tree_path list
  end


    
val make_document_list : ?height:int -> ?show_path:bool
                         -> ?multiple:bool -> ?show_stars:bool ->
                         ?editable:bool -> ?multidrag:bool -> ?library:string ->
                         ?sort:(('a GTree.column) option) -> doc_type:string ->
                         packing:(GObj.widget -> unit) -> (Path.rel * Doc.t) list -> model 

                                                                                                                      
