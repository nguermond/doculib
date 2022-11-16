
exception InitializationError
    
val choose_dir : string -> string option

val error_dialog : string -> unit

class search_bar : GPack.box -> GEdit.entry ->
object
  method init : unit -> unit
  method get_text : unit -> string
  method get_filter : string -> bool
  method on_changed : callback:(unit -> unit) -> unit
end
         
val search_bar : packing:(GObj.widget -> unit) -> search_bar

val edit_notes_dialog : doc:Doc.t -> string
  
val help_dialog : unit -> unit
  
val about_dialog : unit -> unit  
  
val new_library : notebook:Notebook.notebook -> (string * Metadb.Path.root) option
  
val manage_libraries : notebook:Notebook.notebook -> unit

val manage_tags : notebook:Notebook.notebook -> unit
    
val search_metadata : default:Doc.t -> doc_type:string
                      -> search_str:string -> Doc.t option
