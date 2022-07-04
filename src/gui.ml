(******************************************************************************)
(* DocuLib                                                                    *)
(* Copyright (C) 2022 Nathan Guermond                                         *)
(*                                                                            *)
(* This program is free software: you can redistribute it and/or modify it    *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation, either version 3 of the License, or (at your option)  *)
(* any later version.                                                         *)
(*                                                                            *)
(* This program is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *)
(* for more details.                                                          *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License along    *)
(* with this program. If not, see <https://www.gnu.org/licenses/>.            *)
(*                                                                            *)
(******************************************************************************)

open StdLabels
open Gobject.Data                      
open Metadb
   
exception InternalError of string
    
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

class search_bar search_box search_entry =
object
  val search_box : GPack.box = search_box

  val filter_func = (fun (model : GTree.model) row ->
    let search_query = search_entry#text in
    (* prerr_endline ("Searching query: "^search_query); *)
    let search_string = (String.concat " " (List.map (fun column -> model#get ~row ~column)
                                              [Model.Attr.title; Model.Attr.authors; Model.Attr.tags; Model.Attr.path])) in
    let open Agrep in
    if search_query = "" then true
    else
      let pat = (pattern ~transl:Iso8859_15.case_and_accent_insensitive search_query) in
      (string_match pat ~numerrs:(if (String.length search_string > 2) then 1 else 0) search_string)
  )
  val search_entry : GEdit.entry = search_entry

  method init () : unit =
    search_entry#set_secondary_icon_stock `FIND;
    search_entry#set_secondary_icon_tooltip_text
      ( "?X      single character\n"
        ^"*       any character sequence\n"
        ^"[X]    character set (eg. [a-z])\n"
        ^"X & Y    conjunction\n"
        ^"X | Y    disjunction\n"
        ^"\\X      character escape")
    
  method get_text () : string =
    search_entry#text
    
  method on_changed ~callback : unit =
    ignore(search_entry#connect#changed ~callback)

  method get_filter = filter_func
end
  
let search_bar ~packing : search_bar =
  let search_box = GPack.hbox ~border_width:8 ~spacing:8 ~packing () in
  let search_entry = GEdit.entry ~packing:(search_box#add) () in
  let sb = (new search_bar search_box search_entry) in
  let _ = sb#init() in
  sb

let help_dialog () : unit =
  let help_w = GWindow.dialog ~title:"Help" ~border_width:8 ~width:500 ~height:500 ~show:true () in
  let swindow = GBin.scrolled_window ~height:500 ~shadow_type:`ETCHED_IN
                  ~vpolicy:`AUTOMATIC ~packing:(help_w#vbox#pack) () in

  let help_text =
    ["# Features";
     " * files can be moved or renamed without losing metadata";
     " * metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN";
     " * search for metadata on 'openlibrary.org' and 'semanticscholar.org'";
     " * manage multiple libraries in different locations";
     " * error permissive search";
     "";
     "# Libraries";
     " * Each library points to a unique directory, and all files in that directory are automatically added to that library. Each library must have a unique document type, book or article, which indicates";
     "   - the type of new items added to the library";
     "   - the default database to search for metadata";
     "   - whether to store DOI or ISBN.";
     " * Libraries can be managed by selecting the 'Library' menu.";
     " * Files can be moved between libraries by selecting the files (use CONTROL and SHIFT keys to select multiple files), and dragging them to a new tab.";
     "";
     "# Editing Metadata";
     " * Right click on an entry for the following options:";
     "   - Open -- opens the file";
     "   - Search Metadata -- The default search query is either the title or the name of the file. It works best to only use the full title as search query, without authors, date, etc.";
     "   - Open DOI -- if the file has a DOI, open the doi in a web browser";
     "   - Delete File -- will delete the physical file and metadata in your library";
     " * Double click on an entry to manually edit.";
    ]
     in
  let buffer = GText.buffer ~text:(String.concat "\n" help_text) () in
  let view = GText.view ~buffer ~editable:false ~wrap_mode:`WORD_CHAR ~packing:(swindow#add) () in
  help_w#add_button_stock `OK `OK;

  (match help_w#run() with
   | `OK | `DELETE_EVENT -> help_w#destroy())

  
let about_dialog () : unit =
  let licenses = ["doculib: GPLv3.0+";
                  "https://github.com/nguermond/doculib\n";
                  "agrep: LGPLv2+";
                  "https://github.com/xavierleroy/ocamlagrep\n";
                  "quodlibet (MultiDragTreeView): GPLv2+";
                  "https://github.com/quodlibet/quodlibet\n";
                  "Gnome-colors-applications-office: GPLv2+";
                  "https://www.gnome-look.org/p/1012497";
                 ] in
  ignore @@
    GWindow.about_dialog ~name:"DocuLib" ~authors:["nguermond"]
      ~website:"https://github.com/nguermond/doculib"
      ~website_label:"source" ~logo:Icons.doculib_icon
      ~comments:"A GUI for managing document metadata for books, textbooks, or articles."
      ~license:(String.concat "\n" licenses) ~show:true ()
  
  
let new_library ~(notebook:Notebook.notebook) : (string * Path.root) option =
  let dialog = GWindow.dialog ~title:"New Library"
                 ~border_width:8 ~width:300 ~height:150 () in

  let instructions_l = GMisc.label ~xpad:8 ~ypad:8
                         ~text:"Choose an existing directory for a new library:"
                         ~packing:(dialog#vbox#pack) () in
  let grid = GPack.grid  ~col_spacings:8 ~row_spacings:8 ~packing:dialog#vbox#pack () in

  let root_path_l = GMisc.label ~text:"Location" ~packing:(grid#attach ~left:0 ~top:0) () in
  let root_path_hbox = GPack.hbox ~spacing:8 ~packing:(grid#attach ~left:1 ~top:0) () in
  let name_l = GMisc.label ~text:"Name" ~packing:(grid#attach ~left:0 ~top:1) () in
  let name_e = GEdit.entry ~packing:(grid#attach ~left:1 ~top:1) () in
  let root_path_e = GMisc.label ~packing:(root_path_hbox#pack) () in
  let root_path_b = GButton.button ~label:"Choose" ~packing:(root_path_hbox#pack) () in
  let doc_type_l = GMisc.label ~text:"Type" ~packing:(grid#attach ~left:0 ~top:2) () in
  let doc_type_combo = GEdit.combo_box_text ~active:0 ~strings:["article"; "book"]
                         ~packing:(grid#attach ~left:1 ~top:2)() in
  
  ignore @@
    root_path_b#connect#clicked ~callback:(fun () ->
        let root_path =
          match (choose_dir "Choose Library Location") with
          | Some path -> path
          | None -> "" in
        root_path_e#set_text root_path;
        let key = (String.split_on_char '/' root_path) in
        let library = (List.nth key ((List.length key) - 1)) in
        (if name_e#text = "" then
           name_e#set_text library)
      );

  dialog#add_button_stock `OK `OK;
  dialog#add_button_stock `CANCEL `CANCEL;
  
  (match (dialog#run()) with
   | `OK ->
      let doc_type = match GEdit.text_combo_get_active doc_type_combo with
        | None -> raise (InternalError "Not possible: doc_type not selected")
        | Some s -> s in
      let root = Path.mk_root (root_path_e#text) in
      let library = name_e#text in
      dialog#destroy();
      (try
         (let lib = (Library.make (Library.doc_type_of_string doc_type)) in
          Db.add_library ~library ~root lib;
          notebook#add_library ~library ~doc_type ~prepend:false;
          notebook#load_library ~library;
          (Some (library, root)))
       with
       | Db.LibraryExists
         -> (error_dialog (Format.sprintf "Library `%s` already exists!" library));
            None
      )
   | `CANCEL | `DELETE_EVENT ->
      dialog#destroy(); None)

  
let manage_libraries ~notebook : unit =
  let dialog = GWindow.dialog ~title:"Manage Libraries"
                 ~width:500 ~height:200 ~resizable:false () in
  let swindow = GBin.scrolled_window ~height:200 ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
                  ~vpolicy:`AUTOMATIC ~packing:(dialog#vbox#add) () in
  let open Gobject.Data in
  let columns = new GTree.column_list in
  let name = columns#add string in
  let path = columns#add string in

  let store = GTree.list_store columns in
  let view = GTree.view ~model:store ~packing:swindow#add() in

  view#set_enable_grid_lines `HORIZONTAL;
  
  let name_renderer,name_values = (GTree.cell_renderer_text
                                     [`EDITABLE true],
                                   ["text",name]) in
  let path_renderer,path_values = (GTree.cell_renderer_text
                                     [`EDITABLE false;
                                      (* `FONT Font.default_font; *)
                                      `YPAD 2;
                                      `HEIGHT (Font.calc_font_height ~widget:view#coerce ~ypad:2 2);
                                      `WRAP_WIDTH 300;
                                      `WRAP_MODE `WORD_CHAR],
                                   ["text",path]) in
                    
  (* save cell edits *)
  ignore @@
    name_renderer#connect#edited ~callback:(fun p str ->
        let row = (store#get_iter p) in
        let column = name in
        let library = (store#get ~row ~column) in
        (* TODO: escaped quotes can be problematic! *)
        let new_name = (Str.global_replace (Str.regexp "/")
                          (Str.quote "\\") str) in
        (prerr_endline ("Rename: "^library^" ~> "^new_name));
        (if (notebook#rename_library ~library new_name) then
           store#set ~row ~column new_name)
      );

  let name_col = GTree.view_column ~title:"Library" ~renderer:(name_renderer,name_values) () in
  let path_col = GTree.view_column ~title:"Path" ~renderer:(path_renderer,path_values) () in
  name_col#set_reorderable(true);

  ignore @@ view#append_column name_col;
  ignore @@ view#append_column path_col;
  
  List.iter (fun (library,_) ->
      let row = store#append() in
      let root_path = Db.get_library_root ~library in
      store#set ~row ~column:name library;
      store#set ~row ~column:path (Path.string_of_root root_path);
    ) (Db.get_library_descriptions());
  

  
  let hbox = GPack.hbox ~border_width:8 ~spacing:8 ~packing:(dialog#vbox#pack ~from:`END)() in  

  let add_b = GButton.button ~label:"Add" ~packing:(hbox#pack ~from:`END) () in
  (* let move_b = GButton.button ~label:"Move" ~packing:(hbox#pack ~from:`END) () in *)
  let remove_b = GButton.button ~label:"Remove" ~packing:(hbox#pack ~from:`END) () in

  ignore @@
    add_b#connect#clicked ~callback:(fun () ->
        match (new_library ~notebook) with
        | Some (library,root) ->
           let row = store#append() in
           store#set ~row ~column:name library;
           store#set ~row ~column:path (Path.string_of_root root);
        | None -> ()
      );

  (* TODO:: Removing a library no longer deletes metadata. 
   * Add checkbox to remove metadata *)
  ignore @@
    remove_b#connect#clicked ~callback:(fun() ->
        let p = (List.nth (view#selection#get_selected_rows) 0) in
        let row = (store#get_iter p) in
        let library = (store#get ~row ~column:name) in
        let root = (Db.get_library_root ~library) in
        let message = (Format.sprintf "Remove Library '%s'?" library) in
        let confirm_dialog = GWindow.message_dialog ~title:"Confirm Removal"
                               ~buttons:GWindow.Buttons.ok_cancel
                               ~message ~message_type:`QUESTION () in
        let chk = GButton.check_button ~label:(Format.sprintf "Delete metadata -- This will remove:\n '%s/.metadata'" (Path.string_of_root root))
                  ~packing:confirm_dialog#vbox#add () in
        (match confirm_dialog#run() with
         | `OK -> (notebook#remove_library ~delete_metadata:chk#active ~library);
                  ignore (store#remove row)
         | _ -> ());
        confirm_dialog#destroy()
      );

  (* TODO:: Move library *)
  (* move_b#connect#clicked ~callback:(fun () ->
   *     (error_dialog "Not yet implemented")
   *   ); *)
  
  dialog#add_button "Ok" `OK;
  (match dialog#run() with
   | _ -> dialog#destroy()
  )
  
let search_metadata ~(default : Doc.t) ~doc_type (search_str : string) : Doc.t option =
  let docs =
    (try Search.search_document doc_type doc_type search_str with
      Search.SearchFailure msg -> (error_dialog msg);[]) in
       
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

  (* TODO: Nast hack *)
  let docs = (List.map (fun doc -> (Path.mk_rel "dummy", doc)) docs) in
  let model = Model.make_document_list ~height:380 ~show_path:false ~show_stars:false
                ~doc_type ~packing:dialog#vbox#pack docs in
  
  (match doc_type with
   | "article" -> database_l1#set_active true
   | "book" -> database_l2#set_active true
   | _ -> failwith "Not possible");
  

  ignore @@
    refresh_b#connect#clicked ~callback:(fun () ->
        let search_str = search_e#text in
        let search_type = (if database_l1#active then "article" else "book") in
        let docs = (try Search.search_document doc_type search_type search_str with
                      Search.SearchFailure msg -> (error_dialog msg); []) in
        model#reset_model();
        (* TODO: Nasty hack *)
        let docs = (List.map (fun doc -> (Path.mk_rel "dummy", doc)) docs) in
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
        let doc : Doc.t =
          { star = false;
            title = (model#get ~row:(model#get_row p) ~column:Model.Attr.title);
            authors = Str.split (Str.regexp "; +")
                        (model#get ~row:(model#get_row p) ~column:Model.Attr.authors);
            year = (model#get ~row:(model#get_row p) ~column:Model.Attr.year);
            doi = (model#get ~row:(model#get_row p) ~column:Model.Attr.doi);
            isbn = (model#get ~row:(model#get_row p) ~column:Model.Attr.isbn);
            tags = default.tags;
          } in Some doc
     | `CANCEL -> None
     | `DELETE_EVENT -> Some default) in

  dialog#destroy();
  ret

let exit () : unit =
  Db.flush_data();
  GMain.quit()

    
let main () =
  ignore @@ GMain.init();
  let window = GWindow.window
                 ~icon:Icons.doculib_icon ~title:"DocuLib" () in
  let vbox = GPack.vbox ~packing:window#add () in
  
  (* Toplevel menu *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let library_menu = factory#add_submenu "Library" in
  let library_factory = new GMenu.factory library_menu in
  let help_menu = factory#add_submenu "Help" in
  let help_factory = new GMenu.factory help_menu in
  let about_menu = factory#add_submenu "About" in
  let about_factory = new GMenu.factory about_menu in

  (* Context menu *)
  let context_menu = GMenu.menu () in
  let context_factory = new GMenu.factory context_menu in

  (* Search bar *)
  let search_bar = search_bar ~packing:(vbox#pack ~from:`START) in

  (* Database *)
  (* Update_db.init(); *)
  Db.init();

  (* Notebook *)
  let notebook = GPack.notebook ~packing:vbox#add () in
  let notebook = new Notebook.notebook notebook context_menu search_bar#get_filter in
  
  let libraries = (Db.get_library_descriptions ()) in
  let libraries = (List.map (fun (name,desc) ->
                       (name,Library.get_doc_type desc
                             |> Library.string_of_doc_type)) libraries) in
  ignore @@ notebook#init libraries;
  
  
  (* Search on edit *)
  search_bar#on_changed ~callback:(fun _ -> notebook#refilter());

  (****************************************************)
  (* Context menu                                     *)
  (****************************************************)
  (* Open selected files *)
  ignore @@
    context_factory#add_item "Open"
      ~callback:(fun _ ->
        notebook#action_on_selected (fun library model row ->
            let path = Path.mk_rel (model#get ~row ~column:Model.Attr.path) in
            let file = (Db.get_file ~library ~path) in
            System.open_file file)
      );
  
  (* Search for metadata *)
  ignore @@
    context_factory#add_item "Search Metadata"
      ~callback:(fun _ ->
        let doc_type = (snd notebook#current_library)#get_doc_type in
        notebook#edit_selected (fun path doc ->
            let search_str =
              (if doc.title = "" then
                 (Str.global_replace (Str.regexp ("[-_\\()]"))
                    " " (Path.string_of_name (Path.get_leaf path)))
               else doc.title) in
            search_metadata ~default:doc ~doc_type search_str)
      );

    (* Open DOI of selected files *)
  ignore @@
    context_factory#add_item "Open DOI"
      ~callback:(fun _ ->
        notebook#action_on_selected (fun library model row ->
            let doi = (model#get ~row ~column:Model.Attr.doi) in
            (if doi = "" then (error_dialog "No DOI for selected entry!")
             else
               let url = ("https://www.doi.org/" ^
                            (model#get ~row ~column:Model.Attr.doi)) in
               System.open_url url))
      );
  
  (* Edit metadata for an entry *)
  (* context_factory#add_item "Edit Entry"
   *   ~callback:(fun _ -> notebook#edit_selected edit_document); *)

  ignore @@ context_factory#add_separator ();

  (* TODO: Copy file name *)

  (* TODO: Copy file location *)
  ignore @@
    context_factory#add_item "Copy File Path"
      ~callback:(fun _ ->
        let clipboard = GtkBase.Clipboard.get Gdk.Atom.clipboard in
        notebook#action_on_selected (fun library model row ->
            let path = Path.mk_rel (model#get ~row ~column:Model.Attr.path) in
            let file = (Db.get_file ~library ~path) in
            (GtkBase.Clipboard.set_text clipboard (Path.string_of_root file)))
      );

  (* Delete physical file *)
  ignore @@
    context_factory#add_item "Delete File"
      ~callback:(fun _ ->
        (* Confirm deletion *)
        let confirm_dialog = GWindow.message_dialog ~title:"Delete Files"
                               ~buttons:GWindow.Buttons.ok_cancel
                               ~message:"Delete selected file(s)?"
                               ~message_type:`QUESTION () in

        (match confirm_dialog#run() with
         | `OK -> notebook#action_on_selected (fun library model row ->
                      let path = Path.mk_rel (model#get ~row ~column:Model.Attr.path) in
                      model#remove row;
                      Db.remove_entry ~library ~path;
                      Db.remove_file ~library ~path)
         | _ -> ());
        confirm_dialog#destroy()
      );

  (****************************************************)
  (* Library factory                                  *)
  (****************************************************)
  (* Refresh library *)
  ignore @@
    library_factory#add_item "Refresh Library"
      ~callback:(fun () ->
        let (library,_) = notebook#current_library in
        notebook#refresh_library ~library
      );

  (* Make new library tab *)
  ignore @@
    library_factory#add_item "New Library"
      ~callback:(fun () -> ignore (new_library ~notebook)
      );

  (* Manage libraries *)
  ignore @@
    library_factory#add_item "Manage Libraries"
      ~callback:(fun () -> (manage_libraries ~notebook)
      );

  (* Export a library to an archive *)
  (* file_factory#add_item "Export Library"
   *   ~callback:(fun () -> ()
   *   ); *)

  (* Import library from archive *)
  (* file_factory#add_item "Import Library"
   *   ~callback:(fun () -> ()
   *   ); *)

  ignore @@ library_factory#add_separator ();

  ignore @@ library_factory#add_item "Quit" ~callback:window#destroy;

  (****************************************************)
  (* Help factory                                     *)
  (****************************************************)
  ignore @@
    help_factory#add_item "Help" ~callback:(fun () ->
        help_dialog()
      );

  (****************************************************)
  (* About factory                                    *)
  (****************************************************)
  ignore @@
    about_factory#add_item "DocuLib" ~callback:(fun () ->
        about_dialog()
      );
  
  ignore @@ window#connect#destroy ~callback:exit;

  window#set_default_size ~width:1200 ~height:500;
  window#show ();
  GMain.main ()
