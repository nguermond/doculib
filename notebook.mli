
exception LibraryDoesNotExist of string
exception ModelNotLoaded of string

class library : string -> string -> GPack.box ->
  object      
    method get_name : string               
    method get_doc_type : string
    method get_page : GPack.box
    method get_model : Model.model
    method is_loaded : bool
    method set_model : Model.model -> unit
  end
                               
class notebook : GPack.notebook -> GMenu.menu -> (GTree.model -> Gtk.tree_iter -> bool)
  -> object
  val notebook : GPack.notebook
  val context_menu : GMenu.menu
  val filter_func : GTree.model -> Gtk.tree_iter -> bool
  val mutable libraries : (string * library) list
                                                 
  method init : ((string * string) list) -> unit
  method add_library : library:string -> doc_type:string -> unit

  method refilter : unit -> unit

  method get_index : library:string -> int

  method current_library : string * library
       
  method set_model : library:string -> model:Model.model -> unit
    
  method load_library : library:string -> unit

  method action_on_selected : action:(string -> Model.model -> Model.row -> unit) -> unit
  method edit_selected : editor:(Db.doc -> Db.doc option) -> unit
end
