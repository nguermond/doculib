
open Utilities

exception LibraryDoesNotExist of string
exception ModelNotLoaded of string
exception Cancel

                                      
let iter_cancel f lst : unit =
  try (List.iter f lst) with
  | Cancel -> ()

                          
class library library doc_type page =
  object
    val library : string = library
    val doc_type : string = doc_type
    val page : GPack.box = page
    val mutable model : Model.model option = None

    method get_name : string = library
    method get_doc_type : string = doc_type
    method get_page : GPack.box = page
    method get_model : Model.model =
      match model with
      | Some m -> m
      | None -> raise (ModelNotLoaded library)

    method set_model (m : Model.model) : unit =
      model <- Some m

    method is_loaded : bool =
      match model with
      | Some _ -> true
      | None -> false
  end
  
class notebook notebook context_menu filter_func = object (self)
  val notebook : GPack.notebook = notebook
  val context_menu : GMenu.menu = context_menu
  val filter_func : GTree.model -> Gtk.tree_iter -> bool = filter_func
  val mutable libraries : (string * library) list = []
   
  method add_library ~library ~doc_type : unit =
    let label = (GMisc.label ~text:library ()) in
    let page = (GPack.vbox ~border_width:8 ~spacing:8
                  ~packing:(fun w ->
                    ignore (notebook#append_page ~tab_label:(label#coerce) w)) ()) in
    let lib = new library library doc_type page in
    libraries <- (library, lib) :: libraries

  method init (libs : (string * string) list) : unit =
    notebook#connect#switch_page ~callback:(fun index ->
        (self#load_library (fst (List.nth libraries index))));
    List.iter (fun (library,doc_type) -> self#add_library ~library ~doc_type) libs

  method get_index ~library : int =
    match (List.assoc_index libraries library) with
    | None -> raise (LibraryDoesNotExist library)
    | Some i -> i

  method current_library : string * library =
    let page = notebook#current_page in
    (List.nth libraries page)
    
  method refilter () : unit =
    let (_,lib) = self#current_library in
    lib#get_model#get_filter#refilter()

  method set_model ~library ~model : unit =
    List.iter (fun (name, lib) ->
        if library = name then
          lib#set_model model)
      libraries
    
  method load_library ~library : unit =
    let lib = (List.assoc library libraries) in
    if lib#is_loaded then () else
      let library = lib#get_name in
      let doc_type = lib#get_doc_type in
      let page = lib#get_page in
      let data = (Db.get_documents ~library) in
      let model = (Model.make_document_list ~multiple:true ~sort:(Some Model.Attr.star)
                     ~editable:true ~library ~doc_type ~packing:page#add data) in
      model#handle_click_events ~context_menu;
      model#get_filter#set_visible_func filter_func;
      self#set_model library model

  method action_on_selected ~action : unit =
    let (library,lib) = self#current_library in
    List.iter (fun p ->
        (action library lib#get_model (lib#get_model#get_row p)))
      lib#get_model#get_view#selection#get_selected_rows

  method edit_selected ~editor : unit =
    let (library,lib) = self#current_library in
    let model = lib#get_model in
    iter_cancel (fun p ->
        let path = (model#get ~row:(model#get_row p) ~column:Model.Attr.path) in
        let doc = Db.get_document ~library ~path in
        let doc = (match (editor doc) with
                   | None -> raise Cancel
                   | Some doc -> doc) in
        Db.set_document ~library ~path doc;
        model#set_entry (model#get_row p) doc)
      model#get_view#selection#get_selected_rows

end
