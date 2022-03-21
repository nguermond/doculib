
open StdLabels
open Gobject.Data

let icon_path = Db.configdir^"/icons/Gnome-colors-applications-office.svg"

let rec get_files ~library (path : string) : string list =
  if not (Sys.is_directory path) then
    [Db.get_rel_path ~library path]
  else
    (List.flatten
       (List.map (fun name -> get_files ~library (path^"/"^name))
          (Array.to_list (Sys.readdir path))))

  
let choose_files (root_path : string) (library : string) : string list =
  let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~title:"Import Documents" () in
  dialog#set_current_folder root_path;
  dialog#add_button_stock `CANCEL `CANCEL;
  dialog#add_button_stock `OPEN `OPEN;
  dialog#set_select_multiple true;
  let files =
    (match dialog#run() with
     | `OPEN ->
        (match dialog#get_filenames with
         | [] -> failwith "No file name!"
         | files -> List.flatten (List.map (get_files ~library) files))
     | `DELETE_EVENT | `CANCEL -> [])
  in
  dialog#destroy();
  files
  
  
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

let error_dialog (msg : string) : unit =
  let error_dialog = GWindow.message_dialog ~title:"Error"
                       ~buttons:GWindow.Buttons.ok
                       ~message:msg
                       ~message_type:`ERROR () in
  (match error_dialog#run() with
   | _ -> error_dialog#destroy()
  )

  
let new_library ~(db:Db.db) ~(notebook:Notebook.notebook) : (string * string) option =
  let dialog = GWindow.dialog ~title:"New Library" ~border_width:8 ~width:500 ~height:200 () in
  let grid = GPack.grid  ~col_spacings:8 ~row_spacings:8 ~packing:dialog#vbox#pack () in

  (* let name_l = GMisc.label ~text:"Name" ~packing:(grid#attach ~left:0 ~top:0) () in
   * let name_e = GEdit.entry ~packing:(grid#attach ~left:1 ~top:0) () in *)
  let root_path_l = GMisc.label ~text:"Location" ~packing:(grid#attach ~left:0 ~top:0) () in
  let root_path_hbox = GPack.hbox ~spacing:8 ~packing:(grid#attach ~left:1 ~top:0) () in
  let root_path_e = GMisc.label ~packing:(root_path_hbox#pack) () in
  let root_path_b = GButton.button ~label:"Choose" ~packing:(root_path_hbox#pack) () in
  let import_dir_check = GButton.check_button ~label:"import all files" ~packing:(grid#attach ~left:1 ~top:1) () in
  let doc_type_l = GMisc.label ~text:"Type" ~packing:(grid#attach ~left:0 ~top:2) () in
  let doc_type_combo = GEdit.combo_box_text ~active:0 ~strings:["article"; "book"] ~packing:(grid#attach ~left:1 ~top:2)() in
  
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
      let doc_type = match GEdit.text_combo_get_active doc_type_combo with
        | None -> failwith "doc_type select: Not possible"
        | Some s -> s in
      let root = root_path_e#text in
      let key = (String.split_on_char '/' root) in
      let library = (List.nth key ((List.length key) - 1)) in
      dialog#destroy();
      let import_dir = import_dir_check#active in
      (try
         (db#add_library ~library ~doc_type ~root;
          notebook#add_library library doc_type;
          (if import_dir then
             let files = (get_files ~library root) in
             ignore (db#import_files ~library ~doc_type files));
          notebook#load_library library;
          (Some (library, root)))
       with Db.LibraryExists
            -> (error_dialog "Library already exists!");
               None
          | Db.InvalidLibrary -> None
      )
   | `CANCEL | `DELETE_EVENT ->
      dialog#destroy();
      None
  )

  
let manage_libraries ~db ~notebook : unit =
  let dialog = GWindow.dialog ~title:"Manage Libraries"
                 ~width:500 ~height:200 () in

  let swindow = GBin.scrolled_window ~height:200 ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
                  ~vpolicy:`AUTOMATIC ~packing:(dialog#vbox#pack) () in
  let open Gobject.Data in
  let columns = new GTree.column_list in
  let name = columns#add string in
  let path = columns#add string in

  let store = GTree.list_store columns in
  let view = GTree.view ~model:store ~packing:swindow#add() in

  view#set_enable_grid_lines `HORIZONTAL;
  
  let name_renderer,name_values = (GTree.cell_renderer_text
                                     [`EDITABLE true;
                                      `WRAP_WIDTH 200;
                                      `WRAP_MODE `WORD_CHAR],
                                   ["text",name]) in
  let path_renderer,path_values = (GTree.cell_renderer_text
                                     [`EDITABLE false;
                                      `WRAP_WIDTH 300;
                                      `WRAP_MODE `WORD_CHAR],
                                   ["text",path]) in
          
  (* 2 rows of text *)
  name_renderer#set_fixed_height_from_font 2;
          
  (* save cell edits *)
  name_renderer#connect#edited ~callback:(fun p str ->
      (prerr_endline ("Editing: "^str));
    );

  let name_col = GTree.view_column ~title:"Library" ~renderer:(name_renderer,name_values) () in
  let path_col = GTree.view_column ~title:"Path" ~renderer:(path_renderer,path_values) () in
  name_col#set_reorderable(true);

  view#append_column name_col;
  view#append_column path_col;
  
  List.iter (fun library ->
      let row = store#append() in
      let root_path = db#get_library_root ~library in
      store#set ~row ~column:name library;
      store#set ~row ~column:path root_path;
    ) (db#get_libraries());

  let hbox = GPack.hbox ~border_width:8 ~spacing:8 ~packing:(dialog#vbox#pack)() in  

  let add_b = GButton.button ~label:"Add" ~packing:(hbox#pack ~from:`END) () in
  let move_b = GButton.button ~label:"Move" ~packing:(hbox#pack ~from:`END) () in
  let remove_b = GButton.button ~label:"Remove" ~packing:(hbox#pack ~from:`END) () in

  add_b#connect#clicked ~callback:(fun () ->
      match (new_library ~db ~notebook) with
      | Some (library,root) ->
         let row = store#append() in
         store#set ~row ~column:name library;
         store#set ~row ~column:path root;
      | None -> ()
    );

  remove_b#connect#clicked ~callback:(fun() ->
      let p = (List.nth (view#selection#get_selected_rows) 0) in
      let row = (store#get_iter p) in
      let library = (store#get ~row ~column:name) in
      
      let confirm_dialog = GWindow.message_dialog ~title:"Confirm Removal"
                             ~buttons:GWindow.Buttons.ok_cancel
                             ~message:("Remove Library: "^library^"?")
                             ~message_type:`QUESTION () in
      (match confirm_dialog#run() with
       | `OK -> (notebook#remove_library ~library);
                ignore (store#remove row)
       | _ -> ());
      confirm_dialog#destroy()
    );

  move_b#connect#clicked ~callback:(fun () ->
      (error_dialog "Not yet implemented")
    );
  
  dialog#add_button "Ok" `OK;
  (match dialog#run() with
   | _ -> dialog#destroy()
  )
  
let search_metadata ~db (default : Db.doc) (search_str : string) : Db.doc option =
  let docs = Search.search_document default.doc_type default.doc_type search_str in
  let dialog = GWindow.dialog ~title:"Search for Metadata"
                 ~width:800 ~height:400 () in

  let hbox = GPack.hbox ~border_width:8 ~spacing:8 ~packing:(dialog#vbox#pack)() in  
  let search_l = GMisc.label ~text:"Query:" ~packing:(hbox#pack) () in
  let search_e = GEdit.entry ~text:search_str ~packing:(hbox#add) () in

  let radio_hbox = GPack.hbox ~border_width:8 ~spacing:8 ~packing:(dialog#vbox#pack)() in
  let refresh_b = GButton.button ~label:"Refresh" ~packing:(radio_hbox#pack ~from:`END) () in
  let database_l1 = GButton.radio_button 
                      ~label:(Search.get_database_name "article")
                      ~packing:(radio_hbox#pack ~from:`END) () in
  let database_l2 = GButton.radio_button
                      ~group:database_l1#group
                      ~label:(Search.get_database_name "book")
                      ~packing:(radio_hbox#pack ~from:`END) () in

  
  let model = Model.make_document_list ~db ~height:380 ~show_path:false ~show_stars:false
                ~doc_type:default.doc_type ~packing:dialog#vbox#pack docs in
  
  (match default.doc_type with
   | "article" -> database_l1#set_active true
   | "book" -> database_l2#set_active true
   | _ -> failwith "Not possible");
  
  
  refresh_b#connect#clicked ~callback:(fun () ->
      let search_str = search_e#text in
      let search_type = (if database_l1#active then "article" else "book") in
      let docs = Search.search_document default.doc_type search_type search_str in
      model#reset_model();
      model#import_documents docs;
      ());

  dialog#add_button "Select" `OK;
  dialog#add_button "Skip" `DELETE_EVENT;
  dialog#add_button_stock `CANCEL `CANCEL;
  
  let ret =
    (match dialog#run() with
     | `OK ->
        let selection = (model#get_selected_rows) in
        let p = (List.nth selection 0) in
        let doc : Db.doc =
          { star = false;
            title = (model#get ~row:(model#get_row p) ~column:Model.Attr.title);
            authors = Str.split (Str.regexp "; +")
                        (model#get ~row:(model#get_row p) ~column:Model.Attr.authors);
            year = (model#get ~row:(model#get_row p) ~column:Model.Attr.year);
            doi = (model#get ~row:(model#get_row p) ~column:Model.Attr.doi);
            isbn = (model#get ~row:(model#get_row p) ~column:Model.Attr.isbn);
            tags = default.tags;
            doc_type = default.doc_type;
            path = default.path;
            hash = default.hash;
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
                    path = doc.path;
                    hash = doc.hash;
                  } in Some doc
             | `CANCEL -> None
             | `DELETE_EVENT -> Some doc) in
  dialog#destroy();
  ret
  
let main () =
  GMain.init();
  let window = GWindow.window ~title:"DocuLib" () in
  (try
     let icon = GdkPixbuf.from_file icon_path in
     window#set_icon (Some icon)
   with _ -> prerr_endline "Could not find icon");
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
      (* prerr_endline ("Searching query: "^search_query); *)
      let search_string = (String.concat " " (List.map (fun column -> model#get ~row ~column)
                                                [Model.Attr.title; Model.Attr.authors; Model.Attr.tags; Model.Attr.path])) in
      let open Agrep in
      if search_query = "" then true
      else
        let pat = (pattern ~transl:Iso8859_15.case_and_accent_insensitive search_query) in
        (string_match pat ~numerrs:(if (String.length search_string > 2) then 1 else 0) search_string)
    ) in

  (* Database *)
  Update_db.init();
  let db = new Db.db Db.current_branch in
  
  (* Notebook *)
  let notebook = GPack.notebook ~packing:vbox#add () in
  let notebook = new Notebook.notebook notebook db context_menu filter_func in

  
  
  let libraries = (List.map (fun lib -> (lib, db#get_library_doc_type lib)) (db#get_libraries())) in
  notebook#init libraries;
  

  (* Search on edit *)
  search_e#connect#changed ~callback:(fun _ ->
      notebook#refilter());

  (****************************************************)
  (* Context menu                                     *)
  (****************************************************)
  (* Open selected files *)
  context_factory#add_item "Open"
    ~callback:(fun _ ->
      notebook#action_on_selected (fun library model row ->
          db#open_doc ~library ~path:(model#get ~row ~column:Model.Attr.path))
    );

  (* Open DOI of selected files *)
  context_factory#add_item "Open DOI"
    ~callback:(fun _ ->
      notebook#action_on_selected (fun library model row ->
          let doi = (model#get ~row ~column:Model.Attr.doi) in
          (if doi = "" then (error_dialog "No DOI for selected entry!")
           else Utilities.Sys.xopen ("https://www.doi.org/" ^ (model#get ~row ~column:Model.Attr.doi))))
    );
  
  (* Edit metadata for an entry *)
  context_factory#add_item "Edit Entry"
    ~callback:(fun _ -> notebook#edit_selected edit_document);
  
  (* Search for metadata *)
  context_factory#add_item "Search Metadata"
    ~callback:(fun _ ->
      notebook#edit_selected (fun doc ->
          let search_str = (if doc.title = "" then doc.path else doc.title) in
          (* TODO: only supports pdf and djvu, but any file extension should work...? *)
          let search_str = Str.global_replace (Str.regexp "\\(.pdf\\)\\|\\(.djvu\\)\\|[-_\\.() ]+") " " search_str in
          search_metadata db doc search_str)
    );
  
  (* Remove entry from database *)
  context_factory#add_item "Remove Entry"
    ~callback:(fun _ ->
      notebook#action_on_selected (fun library model row ->
          let path = (model#get ~row ~column:Model.Attr.path) in
          db#remove_document library path;
          ignore (model#remove row))
    );

  context_factory#add_separator ();

  (* Rename physical file *)
  context_factory#add_item "Rename File"
    ~callback:(fun _ ->
      (error_dialog "Rename File: Not yet implemented")
    );
  
  (* Delete physical file *)
  context_factory#add_item "Delete File"
    ~callback:(fun _ ->
      (* Confirm deletion *)
      let confirm_dialog = GWindow.message_dialog ~title:"Delete Files"
                             ~buttons:GWindow.Buttons.ok_cancel
                             ~message:"Delete selected file(s)?"
                             ~message_type:`QUESTION () in

      (* TODO: Window does not always close??? *)
      (match confirm_dialog#run() with
       | `OK -> notebook#action_on_selected (fun library model row ->
                    let path = (model#get ~row ~column:Model.Attr.path) in
                    db#remove_document library path;
                    model#remove row;
                    let full_path = (db#get_full_path library path) in
                    ignore (Sys.remove path))
       | _ -> prerr_endline "Cancel");
      confirm_dialog#destroy()
    );

  (****************************************************)

  (* Refresh library *)
  file_factory#add_item "Refresh Library"
    ~callback:(fun () ->
      (* notebook#refresh_library *)
      (error_dialog "Refresh Library: Not yet implemented")
    );
  
  (* Import files from directory *)
  file_factory#add_item "Import Files"
    ~callback:(fun () ->
      try
        let (library,lib) = notebook#current_library in
        let root_path = (db#get_library_root ~library) in
        let files = choose_files root_path library in
        let data = (db#import_files ~library ~doc_type:(lib#get_doc_type) files) in
        lib#get_model#import_documents data
      with
        Notebook.NoLibrary -> (error_dialog "Library must be created first!")
    );

  (* Make new library tab *)
  file_factory#add_item "New Library"
    ~callback:(fun () -> ignore (new_library ~db ~notebook)
    );

  (* Manage libraries *)
  file_factory#add_item "Manage Libraries"
    ~callback:(fun () -> (manage_libraries ~db ~notebook)
    );
  
  file_factory#add_separator ();

  file_factory#add_item "Quit" ~callback:window#destroy;
  
  window#connect#destroy ~callback:GMain.quit;

  window#set_default_size ~width:1200 ~height:500;
  window#show ();
  GMain.main ()
