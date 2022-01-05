
open StdLabels
open Gobject.Data

   
exception Cancel
exception LibraryDoesNotExist of string

module List = struct
  include List
  let index (lst : 'a list) (x : 'a) : int option =
    let ind = ref None in
    (List.iteri (fun j y ->
         (if (x = y) && (!ind = None) then ind := (Some j))) lst);
    !ind
end
            
let iter_cancel f lst : unit =
  try (List.iter f lst) with
  | Cancel -> ()
        
      
  
let choose_files (doc_type : string) (model : Model.model) : unit =
  let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~title:"Import Documents" () in
  dialog#set_current_folder !Db.root;
  dialog#add_button_stock `CANCEL `CANCEL;
  dialog#add_button_stock `OPEN `OPEN;
  dialog#set_select_multiple true;
  (match dialog#run() with
   | `OPEN -> (match dialog#get_filenames with
               | [] -> failwith "No file name!"
               | files -> List.iter (Db.import_file model doc_type) files)
   | `DELETE_EVENT | `CANCEL -> ());
  dialog#destroy()

let choose_dir (title : string) : string option =
  let dialog = GWindow.file_chooser_dialog ~action:`SELECT_FOLDER ~title () in
  dialog#add_button_stock `CANCEL `CANCEL;
  dialog#add_button_stock `OPEN `OPEN;
  let path =
    (match dialog#run() with
     | `OPEN -> dialog#filename
     | `DELETE_EVENT | `CANCEL -> None) in
  dialog#destroy();
  path

  
let search_metadata (default : Db.doc) (search_str : string) : Db.doc option =
  let docs = Search.search_document default.doc_type default.doc_type search_str in
  let dialog = GWindow.dialog ~title:"Search for Metadata"
                 ~width:800 ~height:400 () in

  let hbox = GPack.hbox ~border_width:8 ~spacing:8 ~packing:(dialog#vbox#pack)() in  
  let search_l = GMisc.label ~text:"Query:" ~packing:(hbox#pack) () in
  let search_e = GEdit.entry ~text:search_str ~packing:(hbox#add) () in
  let refresh_b = GButton.button ~label:"Refresh" ~packing:(hbox#pack ~from:`END) () in

  let model = make_document_list ~height:380 ~show_path:false ~show_stars:false
            default.doc_type dialog#vbox#pack docs in
  
  let database_l1 = GButton.radio_button 
                      ~label:(Search.get_database_name "article")
                      ~packing:(dialog#action_area#pack ~from:`START) () in
  let database_l2 = GButton.radio_button
                      ~group:database_l1#group
                      ~label:(Search.get_database_name "book")
                      ~packing:(dialog#action_area#pack ~from:`START) () in

  (match default.doc_type with
   | "article" -> database_l1#set_active true
   | "book" -> database_l2#set_active true
   | _ -> failwith "Not possible");
  
  
  refresh_b#connect#clicked ~callback:(fun () ->
      let search_str = search_e#text in
      let search_type = (if database_l1#active then "article" else "book") in
      (prerr_endline ("Searching "^search_type));
      let docs = Search.search_document default.doc_type search_type search_str in
      (prerr_endline ("Results: "^(string_of_int (List.length docs))));
      model#get_store#clear();
      model#append_data default.doc_type docs;
      ());

  dialog#add_button "Select" `OK;
  dialog#add_button "Skip" `DELETE_EVENT;
  dialog#add_button_stock `CANCEL `CANCEL;
  
  let ret =
    (match dialog#run() with
     | `OK ->
        let selection = (model#get_view#selection#get_selected_rows) in
        let p = (List.nth selection 0) in
        let doc : Db.doc =
          { star = false;
            title = (model#get ~row:(model#get_store#get_iter p) ~column:title);
            authors = Str.split (Str.regexp "; +")
                        (model#get ~row:(model#get_store#get_iter p) ~column:authors);
            year = (model#get ~row:(model#get_store#get_iter p) ~column:year);
            doi = (model#get ~row:(model#get_store#get_iter p) ~column:doi);
            isbn = (model#get ~row:(model#get_store#get_iter p) ~column:isbn);
            tags = default.tags;
            doc_type = default.doc_type;
            path = default.path
          } in Some doc
     | `CANCEL -> None
     | `DELETE_EVENT -> Some default) in

  dialog#destroy();
  ret
  
  
let edit_document (doc : Db.doc) : Db.doc option =
  let dialog = GWindow.dialog ~title:"Edit Document" ~border_width:8 () in
  let grid = GPack.grid  ~col_spacings:8 ~row_spacings:8 ~packing:dialog#vbox#pack () in

  let edit_field label row default =
    let label = GMisc.label ~text:label ~packing:(grid#attach ~left:0 ~top:row) () in
    let entry = GEdit.entry ~text:default ~packing:(grid#attach ~left:1 ~top:row) () in
    entry
  in

  (* TODO: Entry completion *)
  (* let (model, col) = model_of_list Gobject.Data.string ["example 1" ; "example 2"] in
   * let c = GEdit.entry_completion ~model ~entry:authors () in
   * c#set_text_column col ;
   * c#set_match_func (fun str p -> true); *)
  
  let title = (edit_field "Title" 0 doc.title) in
  let authors = (edit_field "Author(s)" 1 (String.concat "; " doc.authors)) in
  let year = (edit_field "Year" 2 doc.year) in
  let doi = (edit_field "DOI" 3 doc.doi) in
  let isbn = (edit_field "ISBN" 4 doc.isbn) in
  let tags = (edit_field "Tags" 5 (String.concat "; " doc.tags)) in

  let path_l = GMisc.label ~text:"Path" ~packing:(grid#attach ~left:0 ~top:6) () in
  let path_l' = GMisc.label ~text:doc.path ~line_wrap:true ~selectable:true
                  ~packing:(grid#attach ~left:1 ~top:6) () in
  
  dialog#add_button_stock `SAVE `SAVE;
  dialog#add_button "Skip" `DELETE_EVENT;
  dialog#add_button_stock `CANCEL `CANCEL;
    
  let ret = (match dialog#run() with
             | `SAVE ->
                let doc : (Db.doc) =
                  { star = doc.star;
                    title = title#text;
                    authors = (Str.split (Str.regexp "; +") authors#text);
                    year = year#text; (* TODO: only allow numeric! *)
                    doi = doi#text;
                    isbn = isbn#text;
                    tags = (Str.split (Str.regexp "; +") tags#text);
                    doc_type = doc.doc_type;
                    path = doc.path
                  } in Some doc
             | `CANCEL -> None
             | `DELETE_EVENT -> Some doc) in
  dialog#destroy();
  ret


    
let main () =
  GMain.init();
  let icon = GdkPixbuf.from_file "icons/Gnome-colors-applications-office.svg" in
  let window = GWindow.window ~title:"DocuLib" ~icon () in
  let vbox = GPack.vbox ~packing:window#add () in

  (* Toplevel menu *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let file_menu = factory#add_submenu "File" in
  let file_factory = new GMenu.factory file_menu in

  (* Context menu *)
  let context_menu = GMenu.menu () in
  let context_factory = new GMenu.factory context_menu in

  (* Search bar *)
  let search_box = GPack.hbox ~border_width:8 ~spacing:8
                     ~packing:(vbox#pack ~from:`START) () in
  let search_e = GEdit.entry ~packing:(search_box#add) () in
  search_e#set_secondary_icon_stock `FIND;
  search_e#set_secondary_icon_tooltip_text
    ( "?X      single character\n"
     ^"*       any character sequence\n"
     ^"[X]    character set (eg. [a-z])\n"
     ^"X & Y    conjunction\n"
     ^"X | Y    disjunction\n"
     ^"\\X      character escape");
  let filter_func = (fun (model : GTree.model) row ->
      let search_query = search_e#text in
      let search_string = (String.concat " " (List.map (fun column -> model#get ~row ~column) [title; authors; tags; path])) in
      let open Agrep in
      if search_query = "" then true
      else
        let pat = (pattern ~transl:Iso8859_15.case_and_accent_insensitive search_query) in
        (string_match pat ~numerrs:(if (String.length search_string > 2) then 1 else 0) search_string)
    ) in

  (* Notebook *)
  let notebook = GPack.notebook ~packing:vbox#add () in
  let notebook = new notebook notebook context_menu filter_func in
  Db.init ();
  let libraries = (List.map (fun lib -> (lib, Db.get_doc_type lib)) Db.get_libraries()) in
  notebook#init libraries;
  

  (* Search on edit *)
  search_e#connect#changed ~callback:(fun _ ->
      let search_text = search_e#text in
      notebook#refilter());

  (****************************************************)
  (* Context menu                                     *)
  (****************************************************)
  (* Open selected files *)
  context_factory#add_item "Open"
    ~callback:(fun _ -> notebook#open_document ());

  (* Open DOI of selected files *)
  context_factory#add_item "Open DOI"
    ~callback:(fun _ -> notebook#open_doi ());
  
  (* Edit metadata for an entry *)
  context_factory#add_item "Edit Entry"
    ~callback:(fun _ -> notebook#edit_entry ());
  
  (* Search for metadata *)
  context_factory#add_item "Search Metadata"
    ~callback:(fun _ ->
      let page = notebook#current_page in
      let doc_type = (List.nth doc_types page) in
      let model = (List.nth models page) in
      (iter_cancel (fun p ->
           let title = (model#get ~row:(model#get_row p) ~column:title) in
           let path = (model#get ~row:(model#get_row p) ~column:path) in
           let search_str =
             (if title = "" then
                (get_doc_name path)
              else title) in
           let search_str = Str.global_replace (Str.regexp "[-_.() ]+") " " search_str in
           let doc = Db.get_document path in
           let doc =
             (match (search_metadata doc search_str) with
              | None -> raise Cancel
              | Some doc -> doc) in
           Db.set_document path doc;
           model#set_entry (model#get_row p) doc;
           ())
         model#get_view#selection#get_selected_rows));
  
  (* Remove entry from database *)
  context_factory#add_item "Remove Entry"
    ~callback:(fun _ ->
      let page = notebook#current_page in
      let model = (List.nth models page) in
      List.iter (fun row ->
          let path = (model#get ~row ~column:path) in
          let _ = Db.remove_document path in
          ignore (model#remove row))
        (List.map model#get_row model#get_view#selection#get_selected_rows));

  context_factory#add_separator ();

  (* Rename physical file *)
  context_factory#add_item "Rename File"
    ~callback:(fun _ ->
      print_endline "Rename File: To be implemented..."; ());
  
  (* Delete physical file *)
  context_factory#add_item "Delete File"
    ~callback:(fun _ ->
      let page = notebook#current_page in
      let model = (List.nth models page) in
      (* Confirm deletion *)
      let confirm_dialog = GWindow.message_dialog ~title:"Delete Files"
                        ~buttons:GWindow.Buttons.ok_cancel
                        ~message:"Delete selected file(s)?"
                        ~message_type:`QUESTION () in
      (* TODO: Window does not always close??? *)
      (match confirm_dialog#run() with
       | `OK -> List.iter (fun row ->
                    let rel_path = (model#get ~row ~column:path) in
                    let _ = Db.remove_document rel_path in
                    let _ = model#get_store#remove row in
                    let path = !Db.root ^ rel_path in
                    ignore (Sys.remove path))
                  (List.map model#get_row model#get_view#selection#get_selected_rows)
       | _ -> prerr_endline "Cancel");
      confirm_dialog#destroy());
      

  (* Import files from directory *)
  file_factory#add_item "Import Files"
    ~callback:(fun () ->
      let (_,(doc_type, Some model)) = notebook#current_library () in
      choose_fies doc_type model);

  file_factory#add_item "New Library"
    ~callback:(fun () -> notebook#new_library);
  
  file_factory#add_separator ();

  file_factory#add_item "Quit" ~callback:window#destroy;
  
  window#connect#destroy ~callback:GMain.quit;

  window#set_default_size ~width:1200 ~height:500;
  window#show ();
  GMain.main ()
