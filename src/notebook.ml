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
   
exception LibraryDoesNotExist of string
exception ModelNotLoaded of string
exception Cancel
exception NoLibrary

let iter_cancel f lst : unit =
  try (List.iter f lst) with
  | Cancel -> ()

                          
class library library doc_type page label =
  object
    val mutable library : string = library
    val doc_type : string = doc_type
    val page : GPack.box = page
    val label : GMisc.label = label
    val mutable model : Model.t option = None

    method get_name : string = library
    method get_doc_type : string = doc_type
    method get_page : GPack.box = page
    method get_label : GMisc.label = label
    method get_model : Model.t =
      match model with
      | Some m -> m
      | None -> raise (ModelNotLoaded library)

    method rename (name : string) : unit =
      library <- name
              
    method set_model (m : Model.t) : unit =
      model <- Some m

    method is_loaded : bool =
      match model with
      | Some _ -> true
      | None -> false
  end

class notebook notebook context_menu filter_func = object (self)
  val notebook : GPack.notebook = notebook
  val context_menu : GMenu.menu = context_menu
  val filter_func : string -> bool = filter_func
  val mutable libraries : (string * library) list = []

  method init (libs : (string * string) list) : unit =
    (List.iter (fun (library,doc_type) ->
         self#add_library ~library ~doc_type ~prepend:false;
         prerr_endline (string_of_int (List.length libraries)))
       libs);
    (* On page switch *)
    ignore @@
      notebook#connect#switch_page ~callback:(fun index ->
          let n = (List.length libs) in
          if n > 0 then
            (let (library,lib) = (List.nth libraries index) in
             Log.push (Format.sprintf "Switching to library: %s[%d]" library index);
             (self#load_library ~library);
             self#refilter ~library:(Some library) ())
          else ()
        );
    if (List.length libs) > 0 then
      begin
        notebook#goto_page 0;
        let library = (fst (self#current_library)) in
        self#load_library ~library
      end

  method add_library ~library ~doc_type ~prepend : unit =
    Log.push (Format.sprintf "Adding library: %s" library);
    let label = (GMisc.label ~text:library ()) in
    let page = (GPack.vbox ~border_width:8 ~spacing:8
                  ~packing:(fun w ->
                    let add_page = (if prepend then notebook#prepend_page
                               else notebook#append_page) in
                    ignore (add_page ~tab_label:(label#coerce) w)) ()) in

    notebook#set_tab_reorderable page#coerce true;
    ignore @@
      notebook#connect#page_reordered ~callback:(fun page i ->
          (* TODO: Only do this once! *)
          let library = (GMisc.label_cast (notebook#get_tab_label page))#text in
          Log.push (Format.sprintf "Reordered tab: %s -> %d" library i);
          let lib = List.assoc library libraries in
          libraries <- List.remove_assoc library libraries;
          libraries <- Listutil.insert libraries i (library,lib);
          Db.flush_libconfig ~ord:(fst (List.split libraries)) ()
        );


    let lib = new library library doc_type page label in
    self#init_DnD_dest lib;
    
    (if prepend then
       (libraries <- (library, lib) :: libraries)
     else
       (let index = (List.length libraries) in
        Log.push (Format.sprintf "Adding library %s at [%d]" library index);
        libraries <- (Listutil.insert libraries index (library,lib))))

  method private init_DnD_dest (lib : library) : unit =
    let page_label = lib#get_label in
    let library = lib#get_name in
    
    page_label#drag#dest_set Model.dnd_targets ~actions:[`COPY];
    
    ignore @@
      page_label#drag#connect#data_received
        ~callback:(fun ctx ~x ~y data ~info ~time ->
          self#load_library ~library;
          if data#format = 8 then
            (let (from_lib,paths) = Doc.deserialize_description data#data in
             (self#move_documents ~from_lib ~to_lib:library paths);
             ctx#finish ~success:true ~del:false ~time)
          else ctx#finish ~success:false ~del:false ~time
        )

  method private move_documents ~from_lib ~to_lib paths : unit =
    (* 1. move physical files to new location (preserving directory structure)
     * 2. move metadata files to new location (preserving directory structure) *)
    Log.push (Format.sprintf "Moving (%d) files from `%s` to `%s`"
                (List.length paths) from_lib to_lib);
    let model = (self#get_library from_lib)#get_model in
    let model' = (self#get_library to_lib)#get_model in    
    Model.iter model (fun key path () ->
        try (Db.migrate ~from_lib ~to_lib ~path;
             let doc = Db.get ~library:to_lib ~path in
             let missing = Model.is_missing model ~key in
             let duplicate = Model.is_duplicate model ~key in
             let tooltip = Model.get_message model ~key in
             let e = (Model.Entry.make path ~missing ~duplicate doc) in
             let key = (Model.add_entry model' e) in
             (Model.set_message model' ~key tooltip);
             true (* mark entry for deletion *))
        with
          Db.CannotMigrate ->
          Log.push (Format.sprintf "Could not move file: %s"
                      (Path.string_of_rel path));
          false
      ) (List.map (fun x -> prerr_endline (Format.asprintf "File: %a"
                                             Path.pp_rel x);
                              (x,())) paths);
    Db.flush_metadata()
    


  method remove_library ~delete_metadata ~library : unit =
    Log.push (Format.sprintf "Removing library `%s`" library);
    (Db.remove_library ~delete_metadata ~library);
    notebook#remove_page (self#get_index ~library);
    libraries <- (List.remove_assoc library libraries)

  method rename_library ~library new_name : bool =
    Log.push (Format.sprintf "Renaming library %s -> %s"
                library new_name);
    try
      (Db.rename_library ~library new_name;
       Db.flush_libconfig();
       let lib = (List.assoc library libraries) in
       (lib#rename new_name);
       libraries <- Listutil.rename_assoc library new_name libraries;
       let label = lib#get_label in
       label#set_text new_name;
       true)
    with
      Db.LibraryExists -> false
    
  method private get_index ~library : int =
    match (Listutil.assoc_index libraries library) with
    | None -> raise (LibraryDoesNotExist library)
    | Some i -> i

  method current_library : (string * library) =
    let (library,lib) =
      let page = notebook#current_page in
      try (List.nth libraries page) with
        _ -> raise NoLibrary
    in
    (library,lib)
    
  method private get_library library : library =
    List.assoc library libraries
    
  method refilter ?(library=None) () : unit =
    let (library,lib) =
      (match library with
       | None -> self#current_library
       | Some library -> (library, self#get_library library))
    in Model.refilter (lib#get_model)

  method private set_model ~library ~model : unit =
    List.iter (fun (name, lib) ->
        if library = name then
          lib#set_model model)
      libraries
    
  method load_library ~library : unit =
    Log.push (Format.sprintf "Loading library: %s[%d]"
                library (Option.get (Listutil.assoc_index libraries library)));
    let lib = (List.assoc library libraries) in
    if lib#is_loaded then () else
      let library = lib#get_name in
      let doc_type = lib#get_doc_type in
      let page = lib#get_page in
      let data = (Db.get_documents ~library) in
      let model = (Model.make_document_list
                     ~sort:(Some Model.Attr.star) ~library
                     ~doc_type ~packing:page#add data) in
      Model.handle_click_events model ~context_menu;
      Model.set_visible_func model filter_func;
      self#set_model library model;
      (self#refresh_library ~library)

  method refresh_library ~library : unit =
    Log.push (Format.sprintf "Refreshing library %s" library);
    let lib = self#get_library library in
    let new_data = (Db.refresh_library ~library) in
    (Model.import_documents (lib#get_model) new_data);
    (* returns a list of entries with missing files *)
    let missing_docs = (Db.resolve_missing_files ~library) in
    let model = lib#get_model in
    Model.iter model ~action:(fun key path missing ->
        if missing then
          (Model.set_message model ~key
             (Format.sprintf "File is missing");
           Model.flag_missing model ~key true;
          false)
        else true (* entry was remapped; mark entry for deletion *))
      missing_docs;
    let duplicates = (Db.find_duplicates()) in
    (* Display list of duplicates *)
    Model.iter model (fun key path (library,dups) ->
        let msg = Font.pango_quote
                    (String.concat "\n"
                       (List.map (fun (library',path) ->
                            Format.asprintf "%s//%a"
                              library' Path.pp_rel path)
                          dups))
        in
        Model.set_message model ~key
          (Format.sprintf "File is a duplicate:\n%s" msg);
        Model.flag_duplicate model ~key true;
        false)
      ((Seq.filter (fun (path,(library',_)) ->
            (library = library'))
          (Seq.concat_map (fun dups ->
               (Seq.map (fun (library,path) ->
                    (path,(library,dups))) (List.to_seq dups)))
             (List.to_seq duplicates)))
       |> List.of_seq)
    
  method action_on_selected ~action : unit =
    let (library,lib) = self#current_library in
    Model.iter_selected lib#get_model (fun key path ->
        (action library path))

  method edit_selected ~editor : unit =
    let (library,lib) = self#current_library in
    let model = lib#get_model in
    try
      Model.iter_selected model (fun key path ->
          let doc = Db.get ~library ~path in
          let doc = (match (editor path doc) with
                     | None -> raise Cancel
                     | Some doc -> doc) in
          Db.set ~library ~path doc;
          let missing = Model.is_missing model ~key in
          let duplicate = Model.is_duplicate model ~key in
          let e = Model.Entry.make path ~missing ~duplicate doc in
          Model.set_entry model ~key e;
          false)
    with Cancel -> ()
end
