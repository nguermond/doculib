

type cell_renderer =
  CellRenderer : ((#GTree.cell_renderer) * (string * 'b GTree.column) list) -> cell_renderer

type column =
  Str : string GTree.column -> column
| Bool : bool GTree.column -> column

type packing = ?from:Gtk.Tags.pack_type -> ?expand:bool -> ?fill:bool -> ?padding:int -> GObj.widget -> unit

type row = Gtk.tree_iter

         
let dnd_targets : Gtk.target_entry list = [
    (* { target = "GTK_TREE_MODEL_ROW"; flags = []; info = 0}; *)
    (* { target = "text/uri-list"; flags = []; info = 2}; *)
    { target = "STRING"; flags = []; info = 0};
    (* { target = "text/plain"; flags = []; info = 2};
     * { target = "application/x-rootwin-drop"; flags = []; info = 1} *)
  ]


module Attr =
  struct
    open Gobject.Data
    let col_names = ["star"; "authors"; "title"; "year"; "doi"; "isbn"; "tags"; "path"]

    let columns = new GTree.column_list
             
    let star = columns#add boolean
    let authors = columns#add string
    let title = columns#add string
    let year = columns#add string
    let doi = columns#add string
    let isbn = columns#add string
    let tags = columns#add string
    let path = columns#add string

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
    let path = Path.rel_to_string path in
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
    store#set ~row ~column:Attr.path (Path.rel_to_string key)
    
  method import_documents (data : (Path.rel * Doc.t) list) : unit =
    List.iter
      (fun (key,doc) ->
        let row = store#append () in
        self#set_entry ~row key doc)
      data

  (* method flag_entry doc : unit =
   *   store#for_each (fun p _ ->
   *       let row = self#get_row p in
   *       if doc.path = (store#get ~row ~column:Attr.path) then *)
          

  method reset_model () : unit =
    store#clear()
    
  method reset_sort_indicators () : unit =
    for i=0 to num_cols - 1 do
      (view#get_column i)#set_sort_indicator false;
    done
    
  method add_column ~title ~width ?(editable=false) ?(library:string = "")
           ?(cell_renderer:cell_renderer option = None)
           (column : column) : unit =
    assert (library <> "" || (not editable));
    
    num_cols <- num_cols + 1;
    let col : (GTree.view_column) =
      (match cell_renderer, column with
       | None, Str col ->
          let renderer,values = (GTree.cell_renderer_text
                                   [`EDITABLE editable;`YPAD 2;
                                    `HEIGHT (Font.calc_font_height ~widget:view#coerce ~ypad:2 2);
                                    `WRAP_WIDTH width;
                                    `WRAP_MODE `WORD_CHAR],
                                 ["text",col]) in
          
          (* save cell edits *)
          ignore @@
            renderer#connect#edited ~callback:(fun p str ->
                let row = (self#get_row p) in
                let path = Path.mk_rel (store#get ~row ~column:Attr.path) in
                let key = (Attr.get_name (col.index)) in
                let doc = Db.get ~library ~path in
                let doc = Doc.edit_document (Doc.set_attribute key str) doc in
                Db.set ~library ~path doc;
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
         then (col#set_sort_order `ASCENDING)
         else (col#set_sort_order `DESCENDING));
        self#reset_sort_indicators ();
        col#set_sort_indicator true;
        store#set_sort_column_id id col#sort_order
      );
    ignore(view#append_column col)

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
     * of the quodlibet music software (licensed under GPLv2.0):
     * https://github.com/quodlibet/quodlibet
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


                                  
let make_document_list ?(height=400) ?(show_path=true) ?(multiple=false)
      ?(show_stars=true) ?(editable=false) ?(multidrag=false) ?(library:string = "")
      ?(sort : ('a GTree.column) option=None) 
      ~doc_type ~packing data : model =
  assert (library <> "" || (not show_stars && not editable && not multidrag));
  
  let swindow = GBin.scrolled_window
                  ~height ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
                  ~vpolicy:`AUTOMATIC ~packing () in

  let store = GTree.list_store Attr.columns in
  let filter = (GTree.model_filter store) in
  let view = GTree.view (* ~reorderable:true *)
               ~model:filter ~packing:swindow#add() in
  let model = (new model filter store view) in

  view#set_enable_grid_lines `HORIZONTAL;
  (if multiple then view#selection#set_mode `MULTIPLE);
  
  (* Columns *)
  let renderer,values = (GTree.cell_renderer_toggle [], ["active", Attr.star]) in
  ignore @@
    renderer#connect#toggled ~callback:(fun p ->
        let row = (model#get_row p) in
        let value = (not (store#get ~row ~column:Attr.star)) in
        let path = Path.mk_rel (model#get ~row ~column:Attr.path) in
        let doc = Db.get ~library ~path in
        let doc = Doc.edit_document (Star value) doc in
        Db.set ~library ~path doc;
        store#set ~row ~column:Attr.star value);
  let star_cell_renderer = Some (CellRenderer (renderer,values)) in
  
  (if show_stars then
     model#add_column ~title:"Star" ~width:40 ~cell_renderer:star_cell_renderer (Bool Attr.star));
  model#add_column ~title:"Author(s)" ~width:200 ~editable ~library (Str Attr.authors);
  model#add_column ~title:"Title" ~width:400 ~editable ~library (Str Attr.title);
  model#add_column ~title:"Year" ~width:100 ~editable ~library (Str Attr.year);
  model#add_column ~title:"Tags" ~width:200 ~editable ~library (Str Attr.tags);
  (match doc_type with
   | "article" -> model#add_column ~title:"DOI" ~width:80 ~editable ~library (Str Attr.doi)
   | "book" -> model#add_column ~title:"ISBN" ~width:80 ~editable ~library (Str Attr.isbn)
   | _ -> ());
  (if show_path then
     ignore @@ model#add_column ~title:"Path" ~width:200 (Str Attr.path));

  (if multidrag then
     (GtkTree.TreeView.Dnd.enable_model_drag_source
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
            (ctx#set_icon_widget image#coerce ~hot_x:0 ~hot_y:0));
  ));
    
  (match sort with
  | None -> ()
  | Some col -> store#set_sort_column_id col.index (view#get_column col.index)#sort_order);
  
  (model#import_documents data);
  model


    
