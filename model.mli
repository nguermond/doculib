

type cell_renderer =
  CellRenderer : ((#GTree.cell_renderer) * (string * 'b GTree.column) list) -> cell_renderer

type column =
  Str : string GTree.column -> column
| Bool : bool GTree.column -> column

type packing = ?from:Gtk.Tags.pack_type -> ?expand:bool -> ?fill:bool -> ?padding:int -> GObj.widget -> unit

type row = Gtk.tree_iter

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
    val filter : GTree.model_filter
    val mutable store : GTree.list_store
    val view : GTree.view
    val mutable num_cols : int

    method get_filter : GTree.model_filter
    method get_store : GTree.list_store
    method get_view : GTree.view
         
    method get_row : Gtk.tree_path -> row
         
    method set : 'a. row:row -> column:('a GTree.column) -> 'a -> unit
    method get : 'a. row:row -> column:('a GTree.column) -> 'a

    method remove : row:Gtk.tree_iter -> unit
      
    method set_entry : row:Gtk.tree_iter -> Db.doc -> unit
      
    method import_documents : (Db.doc list) -> unit
         
    method reset_model : unit -> unit
         
    method reset_sort_indicators : unit -> unit
         
    method add_column : title:string -> width:int -> ?editable:bool -> ?library:string ->
                        ?cell_renderer:(cell_renderer option) -> column -> unit

    (* Handle click events *)
    method handle_click_events : context_menu:GMenu.menu -> unit

    (* TODO: optimize this by importing a list of files *)
    (* method import_file : lib:string -> doc_type:string -> path:string -> unit *)

  end


    
val make_document_list : ?height:int -> ?show_path:bool -> ?multiple:bool -> ?show_stars:bool ->
                         ?editable:bool -> ?library:string ->
                         ?sort:(('a GTree.column) option) -> doc_type:string ->
                         packing:(GObj.widget -> unit) -> (Db.doc list) -> model 

                                                                                                                      