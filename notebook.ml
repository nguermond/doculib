
class notebook notebook context_menu filter_func = object (self)
  val notebook : GPack.notebook = notebook
  val context_menu : GMenu.menu = context_menu
  val filter_func : GTree.model -> Gtk.tree_iter -> bool = filter_func
  val mutable libraries : (string * (string * (Model.model option))) list = []
                                                 
  method init (libs : (string * string) list) : unit =
    notebook#connect#switch_page ~callback:(fun index ->
        let (lib, (doc_type,_)) = (List.nth libraries index) in
        (self#load_library lib doc_type));
      
    libraries <- (List.map (fun lib -> (lib,None)) libs);
    List.iter (fun lib ->
        let label = (GMisc.label ~text:lib ()) in
        GPack.vbox ~border_width:8 ~spacing:8
          ~packing:(fun w ->
            ignore (notebook#append_page ~tab_label:(label#coerce) w)) ())
      libs

  method add_library (lib : string) (doc_type : string) : unit =
    libraries <- (lib, (doc_type,None)) :: libraries

  method refilter () : unit =
    let page = notebook#current_page in
    let (_,(_,Some model)) = (List.nth libraries page) in
    model#get_filter#refilter()

  method get_index (lib : string) : int =
    match (List.index libraries lib) with
    | None -> raise LibraryDoesNotExist lib
    | Some i -> i

  method current_library () : string * (Mode.model option) =
    let page = notebook#current_page in
    snd (List.nth libraries page)

  method set_model (lib : string) (model : model) : unit =
    List.map (fun (lib',(doc_type,model_opt)) ->
        if lib = lib' then (lib',(doc_type,Some model))
        else (lib',(doc_type,model_opt)))
    
  method load_library (lib : string) (doc_type : string) : unit =
    match (List.assoc lib libraries) with
    | Some _ -> () (* Library is already loaded *)
    | None -> 
       let page = notebook#get_page (self#get_index lib) in
       let data = Db.get_documents lib in
       let model = (Model.make_document_list ~multiple:true ~sort:(Some star) doc_type page#add data) in
       model#handle_click_events context_menu;
       model#get_filter#set_visible_func filter_func;
       self#set_model lib model

  method action_on_selected (action : string -> Model.model -> Model.row -> unit) : unit =
    let (lib, Some model) = self#current_library in
    List.iter (fun p ->
        (action lib model (model#get_row)))
      model#get_view#selection#get_selected_rows

  method edit_selected (doc_editor : doc -> doc option) : unit =
    let (lib,Some model) = self#current_library () in
    iter_cancel (fun p ->
        let path = (model#get ~row:(model#get_row p) ~column:path) in
        let doc = Db.get_document path in
        let doc = (match (doc_editor doc) with
                   | None -> raise Cancel
                   | Some doc -> doc) in
        Db.set_document path doc;
        model#set_entry (model#get_row p) doc)
      model#get_view#selection#get_selected_rows

end
