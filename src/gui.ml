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
(* open Dialogs *)

exception InitializationError
   
let quit () : unit =
  Db.flush_metadata();
  Db.flush_libconfig();
  GMain.quit()

(* Init Database *)  
let init_db () : (string * Library.t) list =
  (try Db.init() with
   | Db.InitializationError msg ->
      (Dialogs.error_dialog msg);
      raise InitializationError)




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
  let tag_menu = factory#add_submenu "Tags" in
  let tag_factory = new GMenu.factory tag_menu in
  let view_menu = factory#add_submenu "View" in
  let view_factory = new GMenu.factory view_menu in
  let help_menu = factory#add_submenu "Help" in
  let help_factory = new GMenu.factory help_menu in
  let about_menu = factory#add_submenu "About" in
  let about_factory = new GMenu.factory about_menu in

  (* Context menu *)
  let context_menu = GMenu.menu () in
  let context_factory = new GMenu.factory context_menu in

  (* Search bar *)
  let search_bar = search_bar ~packing:(vbox#pack ~from:`START) in

  (* Notebook *)
  let notebook = GPack.notebook ~packing:vbox#add () in
  let notebook = new Notebook.notebook notebook context_menu search_bar#get_filter in

  let libraries = init_db() in
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
        notebook#action_on_selected ~action:(fun library path ->
            let file = (Db.get_file ~library ~path) in
            if System.file_exists file then
              System.open_file file
            else
              (Dialogs.error_dialog "File does not exist!");
            false)
      );
  
  (* Search for metadata *)
  ignore @@
    context_factory#add_item "Search Metadata"
      ~callback:(fun _ ->
        let doc_type = (snd notebook#current_library)#get_doc_type in
        notebook#edit_selected (fun path doc ->
            let search_str =
              (if doc.title = "" then
                 (Str.global_replace (Str.regexp ("[-_\\()]\\|\\.[^\\.]+$"))
                    " " (Path.string_of_name (Path.get_leaf_rel path)))
               else doc.title) in
            Dialogs.search_metadata ~default:doc ~doc_type ~search_str)
      );

  (* Open DOI of selected files *)
  ignore @@
    context_factory#add_item "Open DOI"
      ~callback:(fun _ ->
        notebook#action_on_selected ~action:(fun library path ->
            let doi = (Db.get ~library ~path).doi in
            (if doi = "" then (Dialogs.error_dialog "No DOI for selected entry!")
             else System.open_url ("https://www.doi.org/" ^ doi));
            false)
      );

  (* Obtain BibTex from DOI/ISBN *)
  ignore @@
    context_factory#add_item "Copy BibTex"
      ~callback:(fun _ ->
        let clipboard = GtkBase.Clipboard.get Gdk.Atom.clipboard in
        let text = ref "" in        
        notebook#action_on_selected ~action:(fun library path ->
            (match Db.get_doc_type ~library with
             | `Book ->
                (Dialogs.error_dialog "BibTex search unavailable for ISBN")
             | `Article ->
                let doi = (Db.get ~library ~path).doi in
                if doi <> "" then
                  (match (Search.get_bibtex_from_doi doi) with
                   | Some bibtex ->
                      text := (if !text="" then "" else !text^"\n")^bibtex
                   | None -> (Dialogs.error_dialog
                                (Format.sprintf "Could not find BibTex for entry `%s`"
                                   (Path.string_of_rel path))))
                else
                  (Dialogs.error_dialog "No DOI for selected entry!"));
            false);
        (GtkBase.Clipboard.set_text clipboard !text)
      );

  (* Edit notes *)
  ignore @@
    context_factory#add_item "Edit Notes"
      ~callback:(fun _ ->
        notebook#edit_selected (fun path doc ->
            let notes = Dialogs.edit_notes_dialog ~doc in
            let doc = Doc.edit_document (Doc.set_attribute "notes" notes) doc in
            Some doc)
      );

  ignore @@ context_factory#add_separator ();

  (* Copy file name to clipboard *)
  ignore @@
    context_factory#add_item "Copy File Name"
      ~callback:(fun _ ->
        let clipboard = GtkBase.Clipboard.get Gdk.Atom.clipboard in
        let text = ref "" in
        notebook#action_on_selected (fun library path ->
            let name = (Path.get_leaf (Db.get_file ~library ~path)) in
            text := (if !text="" then "" else !text^"\n")^(Path.string_of_name name);
            false);
        (GtkBase.Clipboard.set_text clipboard !text)
      );

  (* Copy file location to clipboard *)
  ignore @@
    context_factory#add_item "Copy File Path"
      ~callback:(fun _ ->
        let clipboard = GtkBase.Clipboard.get Gdk.Atom.clipboard in
        let text = ref "" in
        notebook#action_on_selected (fun library path ->
            let file = (Db.get_file ~library ~path) in
            text := (if !text="" then "" else !text^"\n")^(Path.string_of_root file);
            false);
          (GtkBase.Clipboard.set_text clipboard !text)
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
         | `OK -> notebook#action_on_selected (fun library path ->
                      Db.remove_entry ~library ~path;
                      Db.remove_file ~library ~path;
                      true)
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
      ~callback:(fun () -> ignore (Dialogs.new_library ~notebook)
      );

  ignore @@
    library_factory#add_item "New Abstract Library"
      ~callback:(fun () -> ignore (Dialogs.new_abstract_library ~notebook)
      );

  (* Manage libraries *)
  ignore @@
    library_factory#add_item "Manage Libraries"
      ~callback:(fun () -> (Dialogs.manage_libraries ~notebook)
      );

  (****************************************************)
  (* Tag factory                                      *)
  (****************************************************)
  (* Manage tags *)
  ignore @@
    tag_factory#add_item "Tag Relations"
      ~callback:(fun () -> (Dialogs.manage_tags ~notebook)
      );
  
  ignore @@ library_factory#add_separator ();

  ignore @@ library_factory#add_item "Quit" ~callback:window#destroy;

  (****************************************************)
  (* View factory                                     *)
  (****************************************************)
  ignore @@
    view_factory#add_item "Compact" ~callback:(fun () ->
        if (Model.Options.get_row_size () <> 1) then
          begin
            Model.Options.set_row_size 1;
            notebook#reload_libraries();
          end
      );

  ignore @@
    view_factory#add_item "Relaxed" ~callback:(fun () ->
        if (Model.Options.get_row_size () <> 2) then
          begin
            Model.Options.set_row_size 2;
            notebook#reload_libraries();
          end
      );


  (****************************************************)
  (* Help factory                                     *)
  (****************************************************)
  ignore @@
    help_factory#add_item "Help" ~callback:(fun () ->
        Dialogs.help_dialog()
      );

  (****************************************************)
  (* About factory                                    *)
  (****************************************************)
  ignore @@
    about_factory#add_item "DocuLib" ~callback:(fun () ->
        Dialogs.about_dialog()
      );
  
  ignore @@ window#connect#destroy ~callback:quit;

  window#set_default_size ~width:1200 ~height:500;
  window#show ();
  GMain.main ()
