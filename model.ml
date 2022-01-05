

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

  (* Handle click events *)
  method handle_click_events (context_menu : GMenu.factory) : unit =
    (* Open selected files with default program on select *)
    view#connect#after#row_activated
      ~callback:(fun _ _ ->
        List.iter (fun p ->
            open_doc (store#get ~row:(self#get_row p) ~column:path))
          view#selection#get_selected_rows);
    
    (* Spawn context menu on right click *)
    view#event#connect#button_press
      ~callback:(fun ev ->
        (if (GdkEvent.Button.button ev) == 3 then
           (context_menu#popup ~button:3 ~time:(GdkEvent.Button.time ev);
            (if (view#selection#count_selected_rows) > 1
             then true (* do not deselect *)
             else false (* select on right click *)))
         else false))

end


                                  
let make_document_list ?(height=400) ?(show_path=true) ?(multiple=false) ?(show_stars=true)
      ?(sort : ('a GTree.column) option=None) doc_type packing data : model =
  let swindow = GBin.scrolled_window
                  ~height ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
                  ~vpolicy:`AUTOMATIC ~packing () in

  let store = GTree.list_store cols in
  let filter = (GTree.model_filter store) in
  let view = GTree.view ~reorderable:true
               ~model:filter ~packing:swindow#add() in
  let model = (new Model.model filter store view) in

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

  (match sort with
  | None -> ()
  | Some col -> store#set_sort_column_id col.index (view#get_column col.index)#sort_order);
  
  model#append_data doc_type data;
  model
