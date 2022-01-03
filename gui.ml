
open StdLabels
open Gobject.Data

exception Cancel

let iter_cancel f lst : unit =
  try (List.iter f lst) with
  | Cancel -> ()
        
let col_names = ["star"; "authors"; "title"; "year"; "doi"; "isbn"; "tags"; "path"]
               
let cols = new GTree.column_list
let star = cols#add boolean
let authors = cols#add string
let title = cols#add string
let year = cols#add string
let doi = cols#add string
let isbn = cols#add string
let tags = cols#add string
let path = cols#add string


type cell_renderer =
  CellRenderer : ((#GTree.cell_renderer) * (string * 'b GTree.column) list) -> cell_renderer

type column =
  Str : string GTree.column -> column
| Bool : bool GTree.column -> column


class model filter store view = object (self)
  val filter : GTree.model_filter = filter
  val mutable store : GTree.list_store = store
  val view : GTree.view = view
  val mutable num_cols = 0

  method get_filter = filter
  method get_store = store
  method get_view = view
                  
  method get_row p =
    (store#get_iter (filter#convert_path_to_child_path p))

  method set : 'a. row:Gtk.tree_iter -> column:('a GTree.column) -> 'a -> unit =
    store#set

  method get : 'a. row:Gtk.tree_iter -> column:('a GTree.column) -> 'a =
    store#get

  method remove (row : Gtk.tree_iter) : unit =
    ignore (store#remove row)
    
  method set_entry row (doc : Db.doc) : unit =
    let open Db in
    store#set ~row ~column:star doc.star;
    store#set ~row ~column:title doc.title;
    store#set ~row ~column:authors (String.concat "; " doc.authors);
    store#set ~row ~column:doi doc.doi;
    store#set ~row ~column:isbn doc.isbn;
    store#set ~row ~column:year doc.year;
    store#set ~row ~column:tags (String.concat "; " doc.tags);
    store#set ~row ~column:path doc.path
    
  method append_data (doc_type : string) (data : Db.doc list) : unit =
    List.iter
      (fun entry ->
        let open Db in
        (if entry.doc_type = doc_type then
           let row = store#append () in
           self#set_entry row entry))
      data

  method update_model (doc_type : string) : unit =
    let data = Db.get_documents () in
    store#clear();
    self#append_data doc_type data

  method reset_sort_indicators () : unit =
    for i=0 to num_cols - 1 do
      (view#get_column i)#set_sort_indicator false;
    done
    
  method add_column title width ?(editable=true)
           ?(cell_renderer:cell_renderer option = None)
           (column : column) : unit =
    num_cols <- num_cols + 1;
    let col : (GTree.view_column) =
      (match cell_renderer, column with
       | None, Str col ->
          let renderer,values = (GTree.cell_renderer_text
                                   [`EDITABLE editable;
                                    `HEIGHT 36; (* 18 * number of lines *)
                                    `WRAP_WIDTH width;
                                    `WRAP_MODE `WORD_CHAR],
                                 ["text",col]) in
          
          (* 2 rows of text *)
          renderer#set_fixed_height_from_font 2;
          
          (* save cell edits *)
          renderer#connect#edited ~callback:(fun p str ->
              let row = (self#get_row p) in
              let path = (store#get ~row ~column:path) in
              let key = (List.nth col_names (col.index)) in
              let doc = Db.get_document path in
              let doc = Db.edit_document (Db.set_attribute key str) doc in
              (prerr_endline (Format.asprintf "%a" Db.pp_doc doc));
              Db.set_document path doc;
              store#set ~row ~column:col str
            );
          GTree.view_column ~title ~renderer:(renderer,values) ()
       | Some (CellRenderer (renderer,values)), Bool _ ->
          GTree.view_column ~title ~renderer:(renderer,values) ()
       | _ -> failwith "Unexpected column")
    in
    
    col#set_resizable(true);
    col#set_min_width(20);
    col#set_reorderable(true);
    col#set_clickable(true);
    col#set_fixed_width width;
    col#set_sort_order `DESCENDING;

    col#connect#clicked ~callback:(fun () ->
        let title = col#title in
        let id = (match column with Str c -> c.index | Bool c -> c.index) in
        (if col#sort_order = `DESCENDING
         then (prerr_endline "Desc"; col#set_sort_order `ASCENDING)
         else (prerr_endline "Asc"; col#set_sort_order `DESCENDING));
        self#reset_sort_indicators ();
        col#set_sort_indicator true;
        store#set_sort_column_id id col#sort_order
      );
    ignore(view#append_column col)
end
                              
                              
let get_rel_path (path : string) : string =
  (String.sub path (String.length !Db.root)
     ((String.length path) - (String.length !Db.root)))
  
let get_doc_name (path : string) : string =
  let s_path = (String.split_on_char '/' path) in
  let name = (List.nth s_path ((List.length s_path) - 1)) in
  let name = (String.split_on_char '.' name) in
  (List.nth name 0)
  
let open_doc (rel_path : string) : unit =
  let path = (!Db.root ^ rel_path) in
  let ret = Sys.command ("xdg-open \""^path^"\"") in
  (if ret > 0 then
     prerr_endline (path ^ " could not be opened!"));
  ()

let rename_doc (rel_path : string) (new_name : string) : unit =
  let path = (!Db.root ^ rel_path) in
  let rpath = (List.tl (List.rev (String.split_on_char '/' path))) in
  let new_path = (!Db.root ^ (String.concat "/" (List.rev (new_name :: rpath)))) in
  (prerr_endline ("Old path: "^path));
  (prerr_endline ("New path: "^new_path));
  if (Sys.file_exists new_path) then
    prerr_endline ("Cannot rename file - another file of the same name exists!")
  else (Sys.rename path new_path)
(* else (Sys.command ("mv \""^path^"\" \""^new_path^"\"") *)
    
let rec import_file (model : model) (doc_type : string) (path : string) : unit =
  if not (Sys.is_directory path) then
    (let rel_path = get_rel_path path in
     match (Db.import_file rel_path doc_type) with
     | Some doc -> ignore (model#append_data doc_type [doc])
     | None -> ())
  else
    (List.iter (fun name ->
         let path = path^"/"^name in
         import_file model doc_type path)
       (Array.to_list (Sys.readdir path)))
  
let import_dir (doc_type : string) (model : model) () =
  let dialog = GWindow.file_chooser_dialog
                 ~action:`OPEN
                 ~title:"Import"
                 () in
  dialog#set_current_folder !Db.root;
  dialog#add_button_stock `CANCEL `CANCEL;
  dialog#add_button_stock `OPEN `OPEN;
  dialog#set_select_multiple true;
  (match dialog#run() with
   | `OPEN -> (match dialog#get_filenames with
               | [] -> failwith "No file name!"
               | files -> List.iter (import_file model doc_type) files)
   | `DELETE_EVENT | `CANCEL -> ());
  dialog#destroy()


let make_document_list ?(height=400) ?(show_path=true) ?(multiple=false) ?(show_stars=true)
      ?(sort : ('a GTree.column) option=None) doc_type packing data : model =
  let swindow = GBin.scrolled_window
                  ~height ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
                  ~vpolicy:`AUTOMATIC ~packing () in

  let store = GTree.list_store cols in
  let filter = (GTree.model_filter store) in
  let view = GTree.view ~reorderable:true
               ~model:filter ~packing:swindow#add() in
  let model = (new model filter store view) in

  view#set_enable_grid_lines `HORIZONTAL;
  (if multiple then view#selection#set_mode `MULTIPLE);
  
  (* Columns *)
  let renderer,values = (GTree.cell_renderer_toggle [], ["active", star]) in
  renderer#connect#toggled ~callback:(fun p ->
      let row = (model#get_row p) in
      let value = (not (store#get ~row ~column:star)) in
      let path = model#get ~row ~column:path in
      let doc = Db.get_document path in
      let doc = Db.edit_document (Star value) doc in
      Db.set_document path doc;
      store#set ~row ~column:star value);
  
  (if show_stars then
     model#add_column "Star" 40 ~cell_renderer:(Some (CellRenderer (renderer,values))) (Bool star));
  model#add_column "Author(s)" 200 (Str authors);
  model#add_column "Title" 400 (Str title);
  model#add_column "Year" 100 (Str year);
  model#add_column "Tags" 200 (Str tags);
  (match doc_type with
   | "article" -> model#add_column "DOI" 80 (Str doi)
   | "book" -> model#add_column "ISBN" 80 (Str isbn)
   | _ -> ());
  (if show_path then
     ignore(model#add_column "Path" ~editable:false 200 (Str path)));

  (* TODO: sort chosen column *)
  (match sort with
  | None -> ()
  | Some col -> store#set_sort_column_id col.index (view#get_column col.index)#sort_order);
  
  model#append_data doc_type data;
  model
  
let search_metadata (default : Db.doc) (search_str : string) : Db.doc option =
  let docs = Search.search_document default.doc_type default.doc_type search_str in
  let dialog = GWindow.dialog
                 ~title:"Search for Metadata"
                 ~width:800
                 ~height:400 () in

  let hbox = GPack.hbox ~border_width:8 ~spacing:8 ~packing:(dialog#vbox#pack)() in
  
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
  
  
  let search_l = GMisc.label ~text:"Query:"
                   ~packing:(hbox#pack) () in

  let search_e = GEdit.entry ~packing:(hbox#add) () in
  search_e#set_text search_str;
  
  let refresh_b = GButton.button ~label:"Refresh"
                    ~packing:(hbox#pack ~from:`END) () in

  refresh_b#connect#clicked ~callback:(fun () ->
      let search_str = search_e#text in
      let search_type = (if database_l1#active then "article" else "book") in
      (prerr_endline ("Searching "^search_type));
      let docs = Search.search_document default.doc_type search_type search_str in
      (prerr_endline ("Results: "^(string_of_int (List.length docs))));
      model#get_store#clear();
      model#append_data default.doc_type docs;
      ());

  let select_b = GButton.button ~label:"Select"
                   ~packing:(dialog#action_area#pack ~from:`END) () in
  let skip_b = GButton.button ~label:"Skip"
                 ~packing:(dialog#action_area#pack ~from:`END) () in
  let cancel_b = GButton.button ~label:"Cancel"
                   ~packing:(dialog#action_area#pack ~from:`END) () in

  let ret : (Db.doc ref) = ref default in
  
  select_b#connect#clicked ~callback:(fun () ->
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
        } in
      ret := doc;
      dialog#response `DELETE_EVENT; ());

  let cancelled : (bool ref) = ref false in
  skip_b#connect#clicked ~callback:(fun () ->
      dialog#response `DELETE_EVENT; ());
  
  cancel_b#connect#clicked ~callback:(fun () ->
      cancelled := true;
      dialog#response `DELETE_EVENT; ());
  
  dialog#run();
  dialog#destroy();
  (match !cancelled with
   | true -> None
   | false -> Some !ret)

  
(* let model_of_list conv l =
 *   let cols = new GTree.column_list in
 *   let column = cols#add conv in
 *   let model = GTree.list_store cols in
 *   List.iter
 *     (fun data ->
 *       let row = model#append () in
 *       model#set ~row ~column data)
 *     l ;
 *   (model, column) *)
  
let edit_document (doc : Db.doc) : Db.doc option =
  let dialog = GWindow.dialog ~title:"Edit Document" ~modal:false ~border_width:8 () in
  let grid = GPack.grid  ~col_spacings:8 ~row_spacings:8 ~packing:dialog#vbox#pack () in

  let edit_field label row default =
    let label = GMisc.label ~text:label ~packing:(grid#attach ~left:0 ~top:row) () in
    let entry = GEdit.entry ~text:default ~packing:(grid#attach ~left:1 ~top:row) () in
    entry
  in

  let title = (edit_field "Title" 0 doc.title) in
  let authors = (edit_field "Author(s)" 1 (String.concat "; " doc.authors)) in

  (* TODO: Entry completion *)
  (* let (model, col) = model_of_list Gobject.Data.string ["example 1" ; "example 2"] in
   * let c = GEdit.entry_completion ~model ~entry:authors () in
   * c#set_text_column col ;
   * c#set_match_func (fun str p -> true); *)
  
  let year = (edit_field "Year" 2 doc.year) in
  let doi = (edit_field "DOI" 3 doc.doi) in
  let isbn = (edit_field "ISBN" 4 doc.isbn) in
  let tags = (edit_field "Tags" 5 (String.concat "; " doc.tags)) in

  let path_l = GMisc.label ~text:"Path" ~packing:(grid#attach ~left:0 ~top:6) () in
  let path_l' = GMisc.label ~text:doc.path ~line_wrap:true ~selectable:true
                  ~packing:(grid#attach ~left:1 ~top:6) () in
  
  let save_b = GButton.button ~label:"Save" ~packing:(dialog#action_area#pack ~from:`END) () in
  let skip_b = GButton.button ~label:"Skip" ~packing:(dialog#action_area#pack ~from:`END) () in
  let cancel_b = GButton.button ~label:"Cancel" ~packing:(dialog#action_area#pack ~from:`END) () in

  let ret : (Db.doc ref) = ref doc in  
  save_b#connect#clicked ~callback:(fun () ->
      let doc : Db.doc =
        { star = doc.star;
          title = title#text;
          authors = (Str.split (Str.regexp "; +") authors#text);
          year = year#text; (* TODO: only allow numeric! *)
          doi = doi#text;
          isbn = isbn#text;
          tags = (Str.split (Str.regexp "; +") tags#text);
          doc_type = doc.doc_type;
          path = doc.path
        } in
      ret := doc;
      dialog#response `DELETE_EVENT; ());

  let cancelled : (bool ref) = ref false in
  cancel_b#connect#clicked ~callback:(fun () ->
      cancelled := true;
      dialog#response `DELETE_EVENT; ());
  skip_b#connect#clicked ~callback:(fun () ->
      dialog#response `DELETE_EVENT; ());
  
  dialog#run();
  dialog#destroy();
  (match !cancelled with
   | true -> None
   | false -> Some !ret)

  
let main () =
  Db.init ();
  let data = Db.get_documents() in

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

  let search_box = GPack.hbox ~border_width:8 ~spacing:8
                     ~packing:(vbox#pack ~from:`START) () in

  (* Notebook *)
  let notebook = GPack.notebook ~packing:vbox#add () in

  (* Article page *)
  let page1 = 
    GPack.vbox ~border_width:8 ~spacing:8
      ~packing:(fun w ->
        (notebook#append_page
           ~tab_label:((GMisc.label ~text:"Articles"())#coerce) w);())() in

  let model1 = make_document_list ~multiple:true ~sort:(Some star) "article" page1#add data in

  (* Textbook page *)
  let page2 =
    GPack.vbox ~border_width:8 ~spacing:8
      ~packing:(fun w ->
        (notebook#append_page
           ~tab_label:((GMisc.label ~text:"Textbooks"())#coerce) w);
        ())() in

  let model2 = make_document_list ~multiple:true ~sort:(Some star) "book" page2#add data in

  let search_e = GEdit.entry ~packing:(search_box#add) () in
  search_e#set_secondary_icon_stock `FIND;
  search_e#set_secondary_icon_tooltip_text
    ("?X      single character\n"
     ^"X*      any character sequence\n"
     ^"[X]    character set (eg. [a-z])\n"
     ^"X & Y    conjunction\n"
     ^"X | Y    disjunction\n"
     ^"\\X      character escape");
  
  (* Handle click events *)
  let handle_view_events (model : model) () : unit =
    (* Open selected files with default program on select *)
    model#get_view#connect#after#row_activated
      ~callback:(fun _ _ ->
        List.iter (fun p ->
            open_doc (model#get_store#get ~row:(model#get_row p) ~column:path))
          model#get_view#selection#get_selected_rows);
    
    (* Spawn context menu on right click *)
    model#get_view#event#connect#button_press
      ~callback:(fun ev ->
        (if (GdkEvent.Button.button ev) == 3 then
           (context_menu#popup ~button:3 ~time:(GdkEvent.Button.time ev);
            (if (model#get_view#selection#count_selected_rows) > 1
             then true (* select on right click *)
             else false (* do not deselect *)))
         else false)
      );
    ()
  in

  let doc_types = ["article"; "book"] in
  let models = [model1; model2] in

  handle_view_events model1 ();
  handle_view_events model2 ();

  let filter_func = (fun (model : GTree.model) row ->
      let search_query = search_e#text in
      let search_string = (String.concat " " (List.map (fun column -> model#get ~row ~column) [title; authors; tags; path])) in
      let open Agrep in
      if search_query = "" then true
      else
        let pat = (pattern ~transl:Iso8859_15.case_and_accent_insensitive search_query) in
        (string_match pat ~numerrs:(if (String.length search_string > 2) then 1 else 0) search_string)
    ) in

  model1#get_filter#set_visible_func filter_func;
  model2#get_filter#set_visible_func filter_func;
  
  (* Search on edit *)
  search_e#connect#changed ~callback:(fun _ ->
      prerr_endline ("Searching..."^search_e#text);
      let search_text = search_e#text in
      let open Agrep in
      let page = notebook#current_page in
      let model = (List.nth models page) in
      model#get_filter#refilter();
      ());

  
  (* Open selected files *)
  context_factory#add_item "Open"
    ~callback:(fun _ ->
      let page = notebook#current_page in
      let model = (List.nth models page) in
      List.iter (fun p ->
          open_doc (model#get ~row:(model#get_row p) ~column:path))
        model#get_view#selection#get_selected_rows);

  (* Open DOI of selected files *)
  context_factory#add_item "Open DOI"
    ~callback:(fun _ -> prerr_endline "Open DOI: NYI");
  
  (* Edit metadata for an entry *)
  context_factory#add_item "Edit Entry"
    ~callback:(fun _ ->
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
        model#get_view#selection#get_selected_rows);
  
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
      let page = notebook#current_page in
      import_dir (List.nth doc_types page) (List.nth models page) ();
      ());

  file_factory#add_item "New Category"
    ~callback:(fun () ->
      prerr_endline "New Category: To be implemented..."; ());

  
  file_factory#add_separator ();

  file_factory#add_item "Quit" ~callback:window#destroy;
  
  window#connect#destroy ~callback:GMain.quit;

  window#set_default_size ~width:1200 ~height:500;
  window#show ();
  GMain.main ()
