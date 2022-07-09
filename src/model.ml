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
open Metadb

type cell_renderer =
  | CellRenderer : ((#GTree.cell_renderer) * (string * 'b GTree.column) list) -> cell_renderer

type column =
  |  StrCol : string GTree.column -> column
  | BoolCol : bool GTree.column -> column

type packing = ?from:Gtk.Tags.pack_type -> ?expand:bool -> ?fill:bool -> ?padding:int -> GObj.widget -> unit

type store = GTree.list_store

type filter = GTree.model_filter
           
type view = GTree.view
          
type row = Gtk.tree_iter

         
let dnd_targets : Gtk.target_entry list = [
    (* { target = "GTK_TREE_MODEL_ROW"; flags = []; info = 0}; *)
    (* { target = "text/uri-list"; flags = []; info = 2}; *)
    { target = "STRING"; flags = []; info = 0};
    (* { target = "text/plain"; flags = []; info = 2};
     * { target = "application/x-rootwin-drop"; flags = []; info = 1} *)
  ]

(* TODO: Add columns for missing and duplicate files.
 * possibly add tooltip column for message
 *)
module Attr =
  struct
    open Gobject.Data
    let col_names = ["star"; "authors"; "title"; "year"; "doi"; "isbn"; "tags"; "path";
                     (* "fg"; "bg" *)]

    let columns = new GTree.column_list
             
    let star = columns#add boolean
    let authors = columns#add string
    let title = columns#add string
    let year = columns#add string
    let doi = columns#add string
    let isbn = columns#add string
    let tags = columns#add string
    let path = columns#add string
    (* let fg = columns#add boolean
     * let bg = columns#add boolean *)

    let get_name i : string =
      List.nth col_names i
  end

class model filter store view =
object (self)
  val filter : GTree.model_filter = filter
  val mutable store : GTree.list_store = store
  val view : GTree.view = view
  val mutable num_cols = 0
                  
  method get_row p =
    (store#get_iter (filter#convert_path_to_child_path p))

  method get : 'a. row:Gtk.tree_iter -> column:('a GTree.column) -> 'a =
    store#get

  method remove ~row : unit =
    ignore (store#remove row)

  method remove_entry_from_path ~path : unit =
    let path = Path.string_of_rel path in
    (store#foreach (fun p _ ->
         let row = (self#get_row p) in
         let path' = self#get ~row ~column:Attr.path in
         (if path = path' then
            (ignore @@ store#remove row; true)
          else false (* stop searching *))
    ))
    
  method set_entry ~row (key : Path.rel) (doc : Doc.t) : unit =
    store#set ~row ~column:Attr.star doc.star;
    store#set ~row ~column:Attr.title doc.title;
    store#set ~row ~column:Attr.authors (String.concat "; " doc.authors);
    store#set ~row ~column:Attr.doi doc.doi;
    store#set ~row ~column:Attr.isbn doc.isbn;
    store#set ~row ~column:Attr.year doc.year;
    store#set ~row ~column:Attr.tags (String.concat "; " doc.tags);
    store#set ~row ~column:Attr.path (Path.string_of_rel key)
    
  method import_documents (data : (Path.rel * Doc.t) list) : unit =
    List.iter
      (fun (key,doc) ->
        let row = store#append () in
        self#set_entry ~row key doc)
      data

  method reset_model () : unit =
    store#clear()
    
  method reset_sort_indicators () : unit =
    for i=0 to num_cols - 1 do
      (view#get_column i)#set_sort_indicator false;
    done

  method add_column ~title ~width ~(cell_renderer:cell_renderer) (column : column) : unit =
    num_cols <- num_cols + 1;
    let CellRenderer(renderer,values) = cell_renderer in
    let col = (GTree.view_column ~title ~renderer:(renderer,values) ()) in
    
    col#set_resizable(true);
    col#set_min_width(20);
    col#set_reorderable(true);
    col#set_clickable(true);
    col#set_fixed_width width;
    col#set_sort_order `DESCENDING;

    (* Set sort index *)
    ignore @@
      col#connect#clicked ~callback:(fun () ->
          let title = col#title in
          let id = (match column with
                      StrCol c -> c.index
                    | BoolCol c -> c.index) in
          (if col#sort_order = `DESCENDING
           then (col#set_sort_order `ASCENDING)
           else (col#set_sort_order `DESCENDING));
          self#reset_sort_indicators ();
          col#set_sort_indicator true;
          store#set_sort_column_id id col#sort_order
        );
    ignore (view#append_column col)

  (* Handle click events *)
  method handle_click_events ~(context_menu : GMenu.menu) : unit =
    (* Open selected files with default program on select
     * -- conflicts with editing rows 
     *)
    (* view#connect#after#row_activated
     *   ~callback:(fun _ _ ->
     *     List.iter (fun p ->
     *         open_doc (store#get ~row:(self#get_row p) ~column:path))
     *       view#selection#get_selected_rows); *)
    
    (* Click events:
     * - Spawn context menu on right click, do no deselect multiple selection
     * - handle multiple selection for dragging
     * The following code is derived from the MultiDragTreeView 
     * of the quodlibet music software (licensed under GPLv2+):
     *   https://github.com/quodlibet/quodlibet
     * Specifically, see `quodlibet/qltk/views.py`
     * The effect of this is that dragging a multiple selection
     * does not deselect entries. 
     * The key idea here is to use `set_select_function`.
    *)
    let defer_select = ref None in

    (* TODO: If multiple rows are selected, one row is unselected with CONTROL,
     * and then clicked, it is set to edit, not selected, and other rows remain selected!
     *)
    ignore @@
      view#event#connect#button_press
        ~callback:(fun ev ->
          (if (GdkEvent.Button.button ev) = 3 then
             (context_menu#popup ~button:3 ~time:(GdkEvent.Button.time ev);
              (if (view#selection#count_selected_rows) > 1
               then true (* do not deselect *)
               else false (* select on right click *)))
           else if (GdkEvent.Button.button ev) = 1 then
             let x,y = (int_of_float (GdkEvent.Button.x ev),
                        int_of_float (GdkEvent.Button.y ev)) in
             let selection = view#selection in
             let target = view#get_path_at_pos ~x ~y in
             match target with
             | Some (p,col,_,_) ->
                (* if CONTROL or SHIFT not in effect, and entry not selected *)
                if ((((GdkEvent.Button.state ev) land (4 lor 1)) = 0) 
                    && selection#path_is_selected(p)) then
                  (selection#set_select_function (fun p b -> false);
    		       defer_select := Some p; false)
                else (selection#set_select_function (fun p b -> true);
    		          defer_select := None; false)
             | None -> true
           else false)
        );
    ignore @@
      view#event#connect#button_release ~callback:(fun ev ->
          (if (GdkEvent.Button.button ev) = 1 then
             (match !defer_select with
              | Some p ->
                 let selection = view#selection in
                 let x,y = (int_of_float (GdkEvent.Button.x ev),
                            int_of_float (GdkEvent.Button.y ev)) in
                 let target = view#get_path_at_pos ~x ~y in
                 selection#set_select_function (fun p b -> true);
                 (match target with
                  | Some (q,col,_,_) ->
                     (if ((p = q) && (not (x = 0 && y = 0))) then
    		            (view#set_cursor ~edit:(view#selection#count_selected_rows = 1) p col;
                         defer_select := None; false)
                      else
                        (defer_select := None; true))
                  | None -> true)
              | None -> false)
           else false)
        );

  method refilter () : unit =
    filter#refilter()
    
  method set_visible_func (f : GTree.model -> Gtk.tree_iter -> bool) : unit =
    filter#set_visible_func f

  method get_selected_rows : Gtk.tree_path list =
    view#selection#get_selected_rows    
end


let make_toggle_cell_renderer ~(store : store) ~(model : model) ~library ~(column : bool GTree.column) : cell_renderer =
  let renderer,values = (GTree.cell_renderer_toggle [],
                         ["active", column]) in
  ignore @@
    renderer#connect#toggled ~callback:(fun p ->
        let row = (model#get_row p) in
        let value = (not (store#get ~row ~column:Attr.star)) in
        let path = Path.mk_rel (model#get ~row ~column:Attr.path) in
        let doc = Db.get ~library ~path in
        let doc = Doc.edit_document (Star value) doc in
        Db.set ~library ~path doc;
        store#set ~row ~column value);
  CellRenderer(renderer,values)

let make_text_cell_renderer ~(store : store) ~model ~view ~width ~editable ~(library : string option) ~(column : string GTree.column) : cell_renderer =
  let renderer,values =
    (GTree.cell_renderer_text
       [`EDITABLE editable;`YPAD 2;
        `HEIGHT (Font.calc_font_height
                   ~widget:view#coerce ~ypad:2 2);
        `WRAP_WIDTH width;
        `WRAP_MODE `WORD_CHAR;
        `CELL_BACKGROUND_GDK
          (Gdk.Color.color_parse "light yellow");
        `CELL_BACKGROUND_SET false;
        `FOREGROUND_GDK (Gdk.Color.color_parse "red");
        `FOREGROUND_SET false;],
     [("text",column);
      (* TODO: add a column to toggle fg & bg color *)
      (* ("foreground-set", missing);
       * ("background-set", duplicate) *)
    ]) in
  
  (* save cell edits *)
  (match editable, library with
   | true, Some library ->
      ignore @@
        renderer#connect#edited ~callback:(fun p str ->
            let row = (model#get_row p) in
            let path = Path.mk_rel (store#get ~row ~column:Attr.path) in
            let key = (Attr.get_name (column.index)) in
            let doc = Db.get ~library ~path in
            let doc = Doc.edit_document (Doc.set_attribute key str) doc in
            Db.set ~library ~path doc;
            store#set ~row ~column str
          );
   | _ -> ());
  CellRenderer(renderer,values)
  
   
let enable_multidrag ~(store : store) ~(model : model) ~(view : view) ~library : unit =
  GtkTree.TreeView.Dnd.enable_model_drag_source
    view#as_tree_view
    ~modi:[`BUTTON1]
    ~targets:(Array.of_list dnd_targets)
    ~actions:[`COPY];

  ignore @@
    view#drag#connect#data_get ~callback:
      (fun ctx sel ~info ~time ->
        let selection = view#selection#get_selected_rows in
        let paths = (List.map (fun p ->
                         Path.mk_rel @@
                           (store#get ~row:(model#get_row p) ~column:Attr.path))
                       selection) in
        let data = (Doc.serialize_description ~library ~paths) in
        sel#return data);

  ignore @@
    view#drag#connect#after#beginning ~callback:
      (fun ctx ->
        let image = (GMisc.image ~pixbuf:Icons.drag_icon ()) in
        (ctx#set_icon_widget image#coerce ~hot_x:0 ~hot_y:0))

  
let make_document_list ?(height=400) ~(library:string) ~(doc_type:string)
      ?(sort : ('a GTree.column) option=None)
      ~(packing:(GObj.widget -> unit)) (data : (Path.rel * Doc.t) list) : model =  
  let swindow = GBin.scrolled_window
                  ~height ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
                  ~vpolicy:`AUTOMATIC ~packing () in

  let store = GTree.list_store Attr.columns in
  let filter = (GTree.model_filter store) in
  let view = GTree.view (* ~reorderable:true *)
               ~model:filter ~packing:swindow#add() in
  let model = (new model filter store view) in

  view#set_enable_grid_lines `HORIZONTAL;
  view#selection#set_mode `MULTIPLE;
  
  let add_toggle_col ~title ~width col =
    match col with
    | BoolCol(attr) ->
       let cell_renderer = (make_toggle_cell_renderer ~store ~model ~library ~column:attr) in
       model#add_column ~title ~width ~cell_renderer col
    | _ -> failwith "Impossible"
  in
  
  let add_text_col ~title ~width ?(editable=true) col =
    match col with
    | StrCol(attr) ->
       let cell_renderer = (make_text_cell_renderer ~store ~model ~view
                              ~library:(Some library) ~width ~editable ~column:attr) in
       model#add_column ~title ~width ~cell_renderer col
    | _ -> failwith "Impossible"
  in
  
  add_toggle_col ~title:"Star" ~width:40 (BoolCol Attr.star);
  add_text_col ~title:"Authors(s)" ~width:200 (StrCol Attr.authors);                                                                         
  add_text_col ~title:"Title" ~width:400 (StrCol Attr.title);
  add_text_col ~title:"Year" ~width:100 (StrCol Attr.year);
  add_text_col ~title:"Tags" ~width:200 (StrCol Attr.tags);
  (match doc_type with
   | "article" -> add_text_col ~title:"DOI" ~width:80 (StrCol Attr.doi)
   | "book" -> add_text_col ~title:"ISBN" ~width:80 (StrCol Attr.isbn)
   | _ -> ());
  add_text_col ~title:"Path" ~width:200 ~editable:false (StrCol Attr.path);
  
  (enable_multidrag ~store ~model ~view ~library);
  
  (match sort with
   | None -> ()
   | Some col -> store#set_sort_column_id col.index (view#get_column col.index)#sort_order);
  
  (model#import_documents data);
  model

let make_entry_list ?(height=400) ~doc_type ~packing
      ?(sort : ('a GTree.column) option=None)
      data : model =
  
  let swindow = GBin.scrolled_window
                  ~height ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
                  ~vpolicy:`AUTOMATIC ~packing () in

  let store = GTree.list_store Attr.columns in
  let filter = (GTree.model_filter store) in
  let view = GTree.view ~model:filter ~packing:swindow#add() in
  let model = (new model filter store view) in

  view#set_enable_grid_lines `HORIZONTAL;
  

  let add_text_col ~title ~width col =
    match col with
    | StrCol(attr) ->
       let cell_renderer = (make_text_cell_renderer ~store ~model ~view ~width ~library:None ~editable:false ~column:attr) in
       model#add_column ~title ~width ~cell_renderer col
    | _ -> failwith "Impossible"
  in
  (* Columns *)
  add_text_col ~title:"Author(s)" ~width:200 (StrCol Attr.authors);
  add_text_col ~title:"Title" ~width:400 (StrCol Attr.title);
  add_text_col ~title:"Year" ~width:100 (StrCol Attr.year);
  add_text_col ~title:"Tags" ~width:200 (StrCol Attr.tags);
  (match doc_type with
   | "article" -> add_text_col ~title:"DOI" ~width:80 (StrCol Attr.doi)
   | "book" -> add_text_col ~title:"ISBN" ~width:80 (StrCol Attr.isbn)
   | _ -> ());
    
  (match sort with
  | None -> ()
  | Some col -> store#set_sort_column_id col.index (view#get_column col.index)#sort_order);
  
  (model#import_documents data);
  model

    
