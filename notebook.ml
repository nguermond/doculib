
class notebook notebook context_menu filter_func = object (self)
  val notebook : GPack.notebook = notebook
  val context_menu : GMenu.menu = context_menu
  val filter_func : GTree.model -> Gtk.tree_iter -> bool = filter_func
  val mutable libraries : (string * (string * (model option))) list = []
                                                 
  method init (libs : (string * string) list) : unit =
    notebook#connect#switch_page ~callback:(fun index ->
        let (lib, (doc_type,_)) = (List.nth libraries index) in
        (self#load_library lib doc_type))
      
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

  method set_model (lib0 : string) (model : model) : unit =
    List.map (fun (lib,(doc_type,model_opt)) ->
        if lib0 = lib then (lib,(doc_type,Some model))
        else (lib,(doc_type,model_opt)))
      libraries
    
  method load_library (lib : string) (doc_type : string) : unit =
    match (List.assoc lib libraries) with
    | Some _ -> () (* Library is already loaded *)
    | None -> 
       let page = notebook#get_page (self#get_index lib) in
       let data = Db.get_documents lib in
       let model = make_document_list ~multiple:true ~sort:(Some star) doc_type page#add data in
       model#handle_click_events context_menu;
       model#get_filter#set_visible_func filter_func;
       self#set_model lib model

  method new_library () : unit =
    let dialog = GWindow.dialog ~title:"New Library" ~border_width:8 () in
    let grid = GPack.grid  ~col_spacings:8 ~row_spacings:8 ~packing:dialog#vbox#pack () in

    let name_l = GMisc.label ~text:"Name" ~packing:(grid#attach ~left:0 ~top:0) () in
    let name_e = GEdit.entry ~packing:(grid#attach ~left:1 ~top:0) () in
    let root_path_l = GMisc.label ~text:"Location" ~packing:(grid#attach ~left:0 ~top:1) () in
    let root_path_hbox = GPack.hbox ~spacing:8 ~packing:(grid#attach ~left:1 ~top:1) () in
    let root_path_e = GMisc.label ~packing:(root_path_hbox#pack) () in
    let root_path_b = GButton.button ~label:"Choose" ~packing:(root_path_hbox#pack) () in
    let doc_type_l = GMisc.label ~text:"Type" ~packing:(grid#attach ~left:0 ~top:2) () in
    let doc_type_c = GEdit.combo_box_text ~active:0 ~strings:["article"; "book"] ~packing:(grid#attach ~left:1 ~top:2)() in

    root_path_b#connect#clicked ~callback:(fun () ->
        let root_path =
          match (choose_dir "Choose Library Location") with
          | Some path -> path
          | None -> "" in
        root_path_e#set_text root_path);

    dialog#add_button_stock `OK `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    
    (match (dialog#run()) with
     | `OK ->
        let name = name_e#text in
        let doc_type = match GEdit.text_combo_get_active doc_type_c with
          | None -> failwith "doc_type select: Not possible"
          | Some s -> s in
        self#add_library name doc_type;
        self#load_library name doc_type
     | `CANCEL | `DELETE_EVENT ->
        dialog#destroy(); raise Cancel);
    dialog#destroy()

  method open_document () : unit =
    let model = self#current_model in
    List.iter (fun p ->
        open_doc (model#get ~row:(model#get_row p) ~column:path))
      model#get_view#selection#get_selected_rows
    
  method open_doi () : unit =
    prerr_endline "open_doi: NYI"

  method edit_entry () : unit =
    let page = notebook#current_page in
    let model = (List.nth models page) in
    iter_cancel (fun p ->
        let path = (model#get ~row:(model#get_row p) ~column:path) in
        let doc = Db.get_document path in
        let doc = (match (edit_document doc) with
                   | None -> raise Cancel
                   | Some doc -> doc) in
        Db.set_document path doc;
        model#set_entry (model#get_row p) doc)
      model#get_view#selection#get_selected_rows)

end
