
open Utilities

exception LibraryDoesNotExist of string
exception ModelNotLoaded of string
exception Cancel
exception NoLibrary
                                      
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
  
class notebook notebook db context_menu filter_func = object (self)
  val notebook : GPack.notebook = notebook
  val db : Db.db = db
  val context_menu : GMenu.menu = context_menu
  val filter_func : GTree.model -> Gtk.tree_iter -> bool = filter_func
  val mutable libraries : (string * library) list = []
   
  method add_library ~library ~doc_type : unit =
    (prerr_endline ("Adding library "^library));
    let label = (GMisc.label ~text:library ()) in
    let page = (GPack.vbox ~border_width:8 ~spacing:8
                  ~packing:(fun w ->
                    ignore (notebook#prepend_page ~tab_label:(label#coerce) w)) ()) in
    let lib = new library library doc_type page in
    libraries <- (library, lib) :: libraries

  method init (libs : (string * string) list) : unit =
    (List.iter (fun (library,doc_type) -> self#add_library ~library ~doc_type) libs);

    (* On page switch *)
    notebook#connect#switch_page ~callback:(fun index ->
        if (List.length libs) > 0 then
          (let (library,lib) = (List.nth libraries index) in
           (* prerr_endline ("Index:"^ (string_of_int index)^ "->"^library); *)
           (self#load_library library);
           self#refilter ~library:(Some library) ())
        else ()
      );
    (if (List.length libs) > 0 then self#load_library (fst (self#current_library)))

  method private get_index ~library : int =
    match (List.assoc_index libraries library) with
    | None -> raise (LibraryDoesNotExist library)
    | Some i -> i

  method current_library : (string * library) =
    let page = notebook#current_page in
    try (List.nth libraries page) with
      _ -> raise NoLibrary

  method private get_library ~library : library =
    List.assoc library libraries
    
  method refilter ?(library=None) () : unit =
    let (library,lib) =
      (match library with
       | None -> self#current_library
       | Some library -> (library, self#get_library ~library))
    in lib#get_model#refilter()

  method private set_model ~library ~model : unit =
    List.iter (fun (name, lib) ->
        if library = name then
          lib#set_model model)
      libraries
    
  method load_library ~library : unit =
    (prerr_endline ("Loading library "^library));
    let lib = (List.assoc library libraries) in
    if lib#is_loaded then () else
      let _ = (db#check_library_integrity ~library) in
      let library = lib#get_name in
      let doc_type = lib#get_doc_type in
      let page = lib#get_page in
      let data = (db#get_documents ~library) in
      let model = (Model.make_document_list ~db:db ~multiple:true ~sort:(Some Model.Attr.star)
                     ~editable:true ~library ~doc_type ~packing:page#add data) in
      model#handle_click_events ~context_menu;
      model#set_visible_func filter_func;
      self#set_model library model

  method action_on_selected ~action : unit =
    let (library,lib) = self#current_library in
    List.iter (fun p ->
        (action library lib#get_model (lib#get_model#get_row p)))
      lib#get_model#get_selected_rows

  method edit_selected ~editor : unit =
    let (library,lib) = self#current_library in
    let model = lib#get_model in
    iter_cancel (fun p ->
        let path = (model#get ~row:(model#get_row p) ~column:Model.Attr.path) in
        let doc = db#get_document ~library ~path in
        let doc = (match (editor doc) with
                   | None -> raise Cancel
                   | Some doc -> doc) in
        db#set_document ~library ~path doc;
        model#set_entry (model#get_row p) doc)
      model#get_selected_rows

end
