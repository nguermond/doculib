
class notebook
        (notebook : GPack.notebook)
        (context_menu : GMenu.menu)
        (filter_func : GTree.model -> Gtk.tree_iter -> bool)
  = object
  val notebook : GPack.notebook
  val context_menu : GMenu.menu
  val filter_func : GTree.model -> Gtk.tree_iter -> bool
  val mutable libraries : (string * (string * (Model.model option))) list
                                                 
  method init : ~libs:((string * string) list) -> unit
  method add_library : ~lib:string -> ~doc_type:string -> unit

  method refilter : unit -> unit

  method get_index : ~lib:string -> int

  method current_library : unit -> string * (Model.model option)
       
  method set_model : ~lib:string -> ~model:model -> unit
    
  method load_library : ~lib:string -> ~doc_type:string -> unit

  method open_document : ~doc_opener:(string -> string -> unit) -> unit

  method edit_entry : ~editor:(Db.doc -> Db.doc option) -> unit
end
