
exception LibraryDoesNotExist of string
exception ModelNotLoaded of string
exception NoLibrary

class library : string -> string -> GPack.box ->
  object      
    method get_name : string
    method get_doc_type : string
    method get_page : GPack.box
    method get_model : Model.model
    method is_loaded : bool
    method set_model : Model.model -> unit
  end
                               
class notebook : GPack.notebook -> Db.db -> GMenu.menu -> (GTree.model -> Model.row -> bool)
  -> object
  val mutable libraries : (string * library) list
                                                 
  method init : ((string * string) list) -> unit
  method add_library : library:string -> doc_type:string -> unit
  method remove_library : library:string -> unit
  method load_library : library:string -> unit
  method refresh_library : library:string -> unit

  (* method refresh_library_incr : library:string -> (int -> bool) *)

  (* TODO: should not be public *)
  method current_library : string * library
           
  method refilter : ?library:(string option) -> unit -> unit
  method action_on_selected : action:(string -> Model.model -> Model.row -> unit) -> unit
  method edit_selected : editor:(Db.doc -> Db.doc option) -> unit
end
