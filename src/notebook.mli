
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
    method get_model : Model.model
    method rename : string -> unit
    method is_loaded : bool
    method set_model : Model.model -> unit
  end
                               
class notebook : GPack.notebook -> GMenu.menu -> (GTree.model -> Model.row -> bool)
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
  method action_on_selected : action:(string -> Model.model -> Model.row -> unit) -> unit
  method edit_selected : editor:(Path.rel -> Doc.t -> Doc.t option) -> unit
end
