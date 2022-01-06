

type cell_renderer =
  CellRenderer : ((#GTree.cell_renderer) * (string * 'b GTree.column) list) -> cell_renderer

type column =
  Str : string GTree.column -> column
| Bool : bool GTree.column -> column


class model : GTree.model_filter -> GTree.list_store -> GTree.view ->
  object
    val filter : GTree.model_filter
    val mutable store : GTree.list_store
    val view : GTree.view
    val mutable num_cols : int

    method get_filter : unit
    method get_store : unit
    method get_view : unit
         
    method get_row : unit
         
    method set : 'a. row:Gtk.tree_iter -> column:('a GTree.column) -> 'a -> unit
    method get : 'a. row:Gtk.tree_iter -> column:('a GTree.column) -> 'a

    method remove : row:Gtk.tree_iter -> unit
      
    method set_entry : row:Gtk.tree_iter -> doc:Db.doc -> unit
      
    method append_data : doc_type:string -> data:(Db.doc list) -> unit
         
    method update_model : doc_type:string -> unit
         
    method reset_sort_indicators : unit -> unit
         
    method add_column : title:string -> width:int -> ?editable:bool ->
                        ?cell_renderer:(cell_renderer option) -> column:column -> unit

    (* Handle click events *)
    method handle_click_events : context_menu:GMenu.menu -> unit

    (* TODO: optimize this by importing a list of files *)
    method import_file : lib:string -> doc_type:string -> path:string -> unit

  end


    
val make_document_list : ?height:int -> ?show_path:bool -> ?multiple:bool -> ?show_stars:bool ->
                         ?sort:(('a GTree.column) option) -> doc_type:string -> packing:unit -> data:(Db.doc list) -> model 

                                                                                                                      
