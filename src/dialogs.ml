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
open Gobject.Data
open Metadb



exception InternalError of string
exception InitializationError
    
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

  val filter_func = (fun search_string ->
    let search_query = search_entry#text in
    let open Agrep in
    if search_query = "" then true
    else
      let pat = (pattern ~transl:Iso8859_15.case_and_accent_insensitive search_query) in
      (string_match pat ~numerrs:(if (String.length search_string > 2) then 1 else 0)
         search_string)
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

let edit_notes_dialog ~(doc : Doc.t) : string =
  let dialog = GWindow.dialog ~title:"Edit Notes"
                 ~width:400 ~height:400 () in
  let title_l = GMisc.label ~text:doc.title ~line_wrap:true ~width:400
                  ~xpad:8 ~ypad:8 ~packing:(dialog#vbox#pack) () in

  let swindow = GBin.scrolled_window ~shadow_type:`ETCHED_IN
                  ~vpolicy:`AUTOMATIC ~packing:(dialog#vbox#pack ~expand:true)() in
  let buffer = GText.buffer ~text:doc.notes () in
  let view = GText.view ~buffer ~wrap_mode:`WORD_CHAR ~packing:(swindow#add) () in

  dialog#add_button "Ok" `OK;
  dialog#add_button "Cancel" `CANCEL;
  
  (match dialog#run() with
   | `OK ->
      let str = (buffer#get_text()) in
      dialog#destroy();
      str
   | `CANCEL
   | `DELETE_EVENT ->
      dialog#destroy();
      raise Notebook.Cancel)
      

  
let help_dialog () : unit =
  let help_w = GWindow.dialog ~title:"Help" ~border_width:8 ~width:600 ~height:500 ~show:true () in
  let swindow = GBin.scrolled_window  ~shadow_type:`ETCHED_IN
                  ~vpolicy:`AUTOMATIC ~packing:(help_w#vbox#pack ~expand:true) () in
  let help_text = Help.help_text in
  let tag_table = GText.tag_table() in
  let view = GMisc.label ~markup:help_text ~packing:(swindow#add)() in
  help_w#add_button_stock `OK `OK;

  (match help_w#run() with
   | `OK | `DELETE_EVENT -> help_w#destroy())

  
let about_dialog () : unit =
  let licenses = ["doculib: GPLv3.0+";
                  "https://github.com/nguermond/doculib\n";
                  "metadb: GPLv3.0+";
                  "https://github.com/nguermond/metadb\n";
                  "agrep: LGPLv2+";
                  "https://github.com/xavierleroy/ocamlagrep\n";
                  "quodlibet (MultiDragTreeView): GPLv2+";
                  "https://github.com/quodlibet/quodlibet\n";
                  "Gnome-colors-applications-office: GPLv2+";
                  "https://www.gnome-look.org/p/1012497";
                 ] in
  ignore @@
    GWindow.about_dialog ~name:"DocuLib"
      ~authors:["nguermond"]
      ~copyright:"Copyright (C) 2022 Nathan Guermond"
      ~version:"v1.3.3"
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
  let warning_l = GMisc.label ~xpad:8 ~ypad:8
                    ~text:"(Warning: this may take some time for large libraries!)"
                    ~packing:(dialog#vbox#pack)() in
  
  ignore @@
    root_path_b#connect#clicked ~callback:(fun () ->
        let root_path =
          match (choose_dir "Choose Library Location") with
          | Some path -> path
          | None -> "" in
        root_path_e#set_text root_path;
        let default_name = (Path.get_leaf (Path.mk_root root_path)) in
        (if name_e#text = "" then
           name_e#set_text (Path.string_of_name default_name))
      );

  dialog#add_button_stock `OK `OK;
  dialog#add_button_stock `CANCEL `CANCEL;
  
  (match (dialog#run()) with
   | `OK ->
      let doc_type = match GEdit.text_combo_get_active doc_type_combo with
        | None -> raise (InternalError "Not possible: doc_type not selected")
        | Some s -> s in
      let root_txt = (root_path_e#text) in
      let name_txt = Font.pango_quote (name_e#text) in
      dialog#destroy();
      (if root_txt = "" then
         None
       else
         let root = Path.mk_root root_txt in
         let library = name_txt in
         (try
            let lib = (Library.make (Library.doc_type_of_string doc_type)) in
            Db.add_library ~library ~root lib;
            notebook#add_library ~library ~doc_type ~prepend:false;
            notebook#load_library ~library;
            (Some (library, root))
          with
          | Db.LibraryExists
            -> (error_dialog (Format.sprintf "Library `%s` already exists!" library));
               None))
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
        let new_name = str in
        let library = (store#get ~row ~column) in
        (Log.push (Format.sprintf "Rename library: %s -> %s"
                     library new_name));
        if (notebook#rename_library ~library new_name) then
          store#set ~row ~column new_name;
      );

  let name_col = GTree.view_column ~title:"Library"
                   ~renderer:(name_renderer,name_values) () in
  let path_col = GTree.view_column ~title:"Path"
                   ~renderer:(path_renderer,path_values) () in
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

  (* Add checkbox to remove metadata *)
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


let manage_tags ~notebook : unit =
  let dialog = GWindow.dialog ~title:"Tag Relations"
                 ~width:600 ~height:200 ~resizable:false () in
  let swindow = GBin.scrolled_window ~height:200 ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
                  ~vpolicy:`AUTOMATIC ~packing:(dialog#vbox#add) () in
  let open Gobject.Data in
  let columns = new GTree.column_list in

  let rel = columns#add boolean in
  let tag1 = columns#add string in
  let sub = columns#add string in
  let tag2 = columns#add string in

  let store = GTree.list_store columns in
  let view = GTree.view ~model:store ~packing:swindow#add() in

  view#set_enable_grid_lines `HORIZONTAL;
  
  let rel_renderer, rel_values = (GTree.cell_renderer_toggle
                                    [`ACTIVE true],
                                  ["active",rel]) in
  let tag1_renderer,tag1_values = (GTree.cell_renderer_text
                                     [`EDITABLE true],
                                   ["text",tag1]) in
  let sub_renderer,sub_values = (GTree.cell_renderer_text [],
                                   ["text",sub]) in
  let tag2_renderer,tag2_values = (GTree.cell_renderer_text
                                     [`EDITABLE true],
                                   ["text",tag2]) in
                    
  (* save cell edits *)

  let rel_col = GTree.view_column ~title:""
                  ~renderer:(rel_renderer,rel_values) () in

  let tag1_col = GTree.view_column ~title:"Tag 1"
                   ~renderer:(tag1_renderer,tag1_values) () in
  let sub_col = GTree.view_column ~title:""
              ~renderer:(sub_renderer,sub_values)() in
  let tag2_col = GTree.view_column ~title:"Tag 2"
                   ~renderer:(tag2_renderer,tag2_values) () in

  ignore @@ view#append_column tag1_col;
  ignore @@ view#append_column rel_col;
  ignore @@ view#append_column sub_col;  
  ignore @@ view#append_column tag2_col;

  rel_col#set_fixed_width 25;
  sub_col#set_fixed_width 25;
  tag1_col#set_fixed_width 245;
  tag2_col#set_fixed_width 245;

  ignore @@
    rel_renderer#connect#toggled ~callback:(fun p ->
        let row = store#get_iter p in
        let value = not(store#get ~row ~column:rel) in
        store#set ~row ~column:sub (if value then "=" else "≤");
        store#set ~row ~column:rel value
      );
  
  (* load tag relations here *)
  List.iter (fun (tag1',tag2') ->
      let row = store#append() in
      store#set ~row ~column:rel false;
      store#set ~row ~column:tag1 tag1';
      store#set ~row ~column:sub "≤";
      store#set ~row ~column:tag2 tag2';
    ) [];

  dialog#add_button "Ok" `OK;
  (match dialog#run() with
   | _ -> dialog#destroy()
  )

  
let search_metadata ~(default : Doc.t) ~doc_type ~(search_str : string) : Doc.t option =
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

  let docs = (List.mapi (fun i doc -> (Path.mk_rel (string_of_int i), doc)) docs) in
  let model = Model.make_entry_list ~height:380 ~doc_type
                ~packing:dialog#vbox#pack docs in
  
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
        Model.reset_model model;
        let docs = (List.mapi (fun i doc -> (Path.mk_rel (string_of_int i), doc)) docs) in
        Model.import_documents model docs;
        ());

  dialog#add_button "Select" `OK;
  dialog#add_button "Skip" `DELETE_EVENT;
  dialog#add_button_stock `CANCEL `CANCEL;
  
  let ret =
    (match dialog#run() with
     | `OK -> Model.on_selected model (fun key path ->
                  let e = Model.get_entry model ~key in
                  let doc = Model.Entry.get_doc e in
                  let doc = Doc.force_merge doc default in
                  Some doc)
     | `CANCEL -> None
     | `DELETE_EVENT -> Some default) in

  dialog#destroy();
  ret
