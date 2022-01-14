

type cell_renderer =
  CellRenderer : ((#GTree.cell_renderer) * (string * 'b GTree.column) list) -> cell_renderer

type column =
  Str : string GTree.column -> column
| Bool : bool GTree.column -> column

type packing = ?from:Gtk.Tags.pack_type -> ?expand:bool -> ?fill:bool -> ?padding:int -> GObj.widget -> unit

type row = Gtk.tree_iter


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

  method set : 'a. row:Gtk.tree_iter -> column:('a GTree.column) -> 'a -> unit =
    store#set

  method get : 'a. row:Gtk.tree_iter -> column:('a GTree.column) -> 'a =
    store#get

  method remove ~row : unit =
    ignore (store#remove row)
    
  method set_entry ~row doc : unit =
    let open Db in
    store#set ~row ~column:Attr.star doc.star;
    store#set ~row ~column:Attr.title doc.title;
    store#set ~row ~column:Attr.authors (String.concat "; " doc.authors);
    store#set ~row ~column:Attr.doi doc.doi;
    store#set ~row ~column:Attr.isbn doc.isbn;
    store#set ~row ~column:Attr.year doc.year;
    store#set ~row ~column:Attr.tags (String.concat "; " doc.tags);
    store#set ~row ~column:Attr.path doc.path
    
  method import_documents (data : Db.doc list) : unit =
    List.iter
      (fun doc ->
        let row = store#append () in
        self#set_entry ~row doc)
      data

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
          (* TODO: There is a bug in which height of the first entry
           * readjusts after mouseover *)
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
              let path = (store#get ~row ~column:Attr.path) in
              let key = (Attr.get_name (col.index)) in
              let doc = Db.get_document ~library ~path in
              let doc = Db.edit_document (Db.set_attribute key str) doc in
              (prerr_endline (Format.asprintf "%a" Db.pp_doc doc));
              Db.set_document ~library ~path doc;
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
    (* Open selected files with default program on select *)
    (* view#connect#after#row_activated
     *   ~callback:(fun _ _ ->
     *     List.iter (fun p ->
     *         open_doc (store#get ~row:(self#get_row p) ~column:path))
     *       view#selection#get_selected_rows); *)
    
    (* Spawn context menu on right click *)
    ignore
      (view#event#connect#button_press
         ~callback:(fun ev ->
           (if (GdkEvent.Button.button ev) == 3 then
              (context_menu#popup ~button:3 ~time:(GdkEvent.Button.time ev);
               (if (view#selection#count_selected_rows) > 1
                then true (* do not deselect *)
                else false (* select on right click *)))
            else false)))

  method refilter () : unit =
    filter#refilter()
    
  method set_visible_func (f : GTree.model -> Gtk.tree_iter -> bool) : unit =
    filter#set_visible_func f

  method get_selected_rows : Gtk.tree_path list =
    view#selection#get_selected_rows
    
end


                                  
let make_document_list ?(height=400) ?(show_path=true) ?(multiple=false)
      ?(show_stars=true) ?(editable=false) ?(library:string = "")
      ?(sort : ('a GTree.column) option=None) 
      ~doc_type ~packing data : model =
  assert (library <> "" || (not show_stars && not editable));
  
  let swindow = GBin.scrolled_window
                  ~height ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
                  ~vpolicy:`AUTOMATIC ~packing () in

  let store = GTree.list_store Attr.columns in
  let filter = (GTree.model_filter store) in
  let view = GTree.view ~reorderable:true
               ~model:filter ~packing:swindow#add() in
  let model = (new model filter store view) in

  view#set_enable_grid_lines `HORIZONTAL;
  (if multiple then view#selection#set_mode `MULTIPLE);
  
  (* Columns *)
  let renderer,values = (GTree.cell_renderer_toggle [], ["active", Attr.star]) in
  renderer#connect#toggled ~callback:(fun p ->
      let row = (model#get_row p) in
      let value = (not (store#get ~row ~column:Attr.star)) in
      let path = model#get ~row ~column:Attr.path in
      let doc = Db.get_document ~library ~path in
      let doc = Db.edit_document (Star value) doc in
      Db.set_document ~library ~path doc;
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
     ignore(model#add_column ~title:"Path" ~width:200 (Str Attr.path)));

  (match sort with
  | None -> ()
  | Some col -> store#set_sort_column_id col.index (view#get_column col.index)#sort_order);
  
  (model#import_documents data);
  model


    
