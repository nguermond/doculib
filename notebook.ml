
open Utilities

exception LibraryDoesNotExist of string

class notebook notebook context_menu filter_func = object (self)
  val notebook : GPack.notebook = notebook
  val context_menu : GMenu.menu = context_menu
  val filter_func : GTree.model -> Gtk.tree_iter -> bool = filter_func
  val mutable libraries : (string * (string * (Model.model option))) list = []

                                                                          
  method add_library ~library ~doc_type : unit =
    libraries <- (library, (doc_type,None)) :: libraries;
    let label = (GMisc.label ~text:library ()) in
    ignore
      (GPack.vbox ~border_width:8 ~spacing:8
         ~packing:(fun w ->
           ignore (notebook#append_page ~tab_label:(label#coerce) w)) ())

  method init (libs : (string * string) list) : unit =
    notebook#connect#switch_page ~callback:(fun index ->
        let (lib, (doc_type,_)) = (List.nth libraries index) in
        (self#load_library lib));

    List.iter (fun (library,doc_type) -> self#add_library ~library ~doc_type) libs;

  method get_index ~library : int =
    match (List.assoc_index libraries library) with
    | None -> raise (LibraryDoesNotExist library)
    | Some i -> i

  method current_library () : string * (string * Model.model) =
    let page = notebook#current_page in
    match (List.nth libraries page) with
    | (lib,(doc_type,Some model)) -> (lib,(doc_type,model))
    | _ -> failwith "No loaded model"

  method refilter () : unit =
    let (_,(_,model)) = self#current_library () in
    model#get_filter#refilter()

  method set_model ~library ~model : unit =
    libraries <-
      List.map (fun (lib,(doc_type,model_opt)) ->
          if library = lib then (lib,(doc_type,Some model))
          else (lib,(doc_type,model_opt)))
        libraries
    
  method load_library ~library : unit =
    match (List.assoc library libraries) with
    | (_,Some _) -> () (* Library is already loaded *)
    | (doc_type,None) ->
       let page = notebook#get_nth_page (self#get_index ~library) in
       let data = Db.get_documents ~library in
       let model = (Model.make_document_list ~multiple:true ~sort:(Some Model.Attr.star)
                      ~editable:true ~library ~doc_type ~packing:page#add data) in
       model#handle_click_events ~context_menu;
       model#get_filter#set_visible_func filter_func;
       self#set_model library model

  method action_on_selected ~action : unit =
    let (lib, (_,model)) = self#current_library in
    List.iter (fun p ->
        (action lib model (model#get_row)))
      model#get_view#selection#get_selected_rows

  method edit_selected (doc_editor : doc -> doc option) : unit =
    let (lib,(_, model)) = self#current_library () in
    iter_cancel (fun p ->
        let path = (model#get ~row:(model#get_row p) ~column:path) in
        let doc = Db.get_document ~library:lib path in
        let doc = (match (doc_editor doc) with
                   | None -> raise Cancel
                   | Some doc -> doc) in
        Db.set_document path doc;
        model#set_entry (model#get_row p) doc)
      model#get_view#selection#get_selected_rows

end
