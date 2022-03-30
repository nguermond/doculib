open Lwt.Syntax
open Format

(* exception InitializationFailure of string *)
exception LibraryExists
exception InvalidLibrary
exception CannotMoveFile
(* exception FileNotFound of string *)



type doc = Doc.t
open Doc
  
(* We store an absolute path to each library, 
 * and a path to a file is relative to this path. 
 * The path to the file is also its key. 
 * The purpose of having a path for every library is that
 * an entire library can be relocated without having to modify entries. *)

(* The database is versioned, to prevent data loss upon upgrade. *)
let branches = ["2.1"; "2.0"]
let current_branch = "2.1"

let configdir : string = (Sys.getenv "HOME")^"/.doculib"
let datadir : string = configdir^"/data"
let libconfig : string = configdir^"/libraries.json"

type library = {name : string;
                root : string;
                doc_type : string
               }

let get_rel_path ~(library_root : string) (path : string) : string =
  (Str.replace_first (Str.regexp (Str.quote (library_root^"/"))) "" path)

let json_to_libs (json : Json.t) : (string * library) list =
  (Json.to_list (Json.raise_opt "`libraries`"
                   (Json.get "libraries" json)))
  |> List.map (fun json ->
         let name = Json.to_string (Json.raise_opt "`name`" (Json.get "name" json)) in
         let path = Json.to_string (Json.raise_opt "`path`" (Json.get "path" json)) in
         let doc_type = Json.to_string (Json.raise_opt "`doc_type`" (Json.get "doc_type" json)) in
         (name,{name = name; root = path; doc_type = doc_type}))

let get_lib_version () : string =
  let json = Yojson.Basic.from_file libconfig in
  Json.to_string
    (match (Json.get "version" json) with
     | Some v -> v
     | None -> `String "2.0")

let set_lib_version (version : string) : unit =
  let json = Yojson.Basic.from_file libconfig in
  let json = (Json.remove_entry "version" json) in
  let json = (Json.add_entry "version" (`String version) json) in
  (Json.to_file libconfig json)
  

let libs_to_json (version : string) (libs : (string * library) list) : Json.t =
  let libs = (`List (List.map (fun (name,lib) ->
                         (`Assoc [("name",`String lib.name);
                                  ("path", `String lib.root);
                                  ("doc_type", `String lib.doc_type)]))
                       libs)) in
  (`Assoc [("version",`String version);
           ("libraries", libs)])

let init_lib_config () : unit =
  let json = (libs_to_json current_branch []) in
  (Json.to_file libconfig json)


         
class db branch =
object (self)
  val store : string =
    (let store = (datadir^"/"^branch) in
     (if (not (Sys.file_exists store)) then
        (prerr_endline "store directory does not exist: creating...";
         Sys.mkdir store 0o755));
     store)
    
  val mutable libraries : ((string * library) list) =
    let json = Yojson.Basic.from_file libconfig in
    (json_to_libs json)

  val version : string = get_lib_version()
    
(* Store document metadata as
 * store/library/path.json *)  
  method add_document ~library (doc : doc) : unit =
    let name = (store^"/"^library^"/"^doc.path^".json") in
    let json = doc_to_json doc in
    (Utilities.Sys.make_dirs store (library^"/"^doc.path));
    (Json.to_file name json)
  
  method get_document ~library ~path : doc =
    let name = (store^"/"^library^"/"^path^".json") in
    let json = (Json.from_file name) in
    (json_to_doc path json)

  method set_document ~library ~path doc : unit =
    let name = (store^"/"^library^"/"^path^".json") in
    let json = doc_to_json doc in
    (Json.to_file name json)
  
  method remove_document ~library ~path : unit =
    let name = (store^"/"^library^"/"^path^".json") in
    Sys.remove name

  method move_document ~from_lib ~to_lib ~path : unit =
    let old_md = (from_lib^"/"^path^".json") in
    let new_md = (to_lib^"/"^path^".json") in
    let old_root = (self#get_library_root ~library:from_lib) in
    let new_root = (self#get_library_root ~library:to_lib) in
    (if Sys.file_exists (new_root^"/"^path) then (raise CannotMoveFile));
    (Utilities.Sys.make_dirs store new_md);
    (Utilities.Sys.make_dirs new_root path);
    (Utilities.Sys.move (store^"/"^old_md) (store^"/"^new_md));
    (Utilities.Sys.move (old_root^"/"^path) (new_root^"/"^path))
    
  method private import_file ~library path : doc option =
    let name = (store^"/"^library^"/"^path^".json") in
    let doc_type = (self#get_library_doc_type ~library) in
    (if (not (Sys.file_exists name)) then
       (let full_path = self#get_full_path ~library path in
        let doc = (make_doc_from_file full_path path doc_type) in
        let _ = self#add_document ~library doc in
        (Some doc))
     else None)
    
  method private import_files ~library (paths : string list) : doc list =
    (prerr_endline "Importing files");
    List.filter_map (fun path ->
        (* (prerr_endline ("Loading~"^path)); *)
        (self#import_file ~library path))
      paths

  method refresh_library ~library : doc list =
    let library_root = self#get_library_root ~library in
    let map = (get_rel_path ~library_root) in
    let paths = Utilities.Sys.get_files library_root ~map in
    (self#import_files ~library paths)

  method get_documents ~library : doc list =
    let library_root = (store^"/"^library) in
    let map = (fun path ->
        let path = (Str.replace_first (Str.regexp (Str.quote ".json")) "" path) in
        let path = get_rel_path ~library_root path in
        self#get_document ~library ~path) in
    (* let map = self#get_document_from_file in *)
    (Utilities.Sys.get_files (store^"/"^library) ~map)
  
  method private print_documents library : unit =
    let docs = self#get_documents ~library in
    List.iter (fun d -> printf "%a@\n" pp_doc d) docs
    
  method get_library_root ~library : string =
    (List.assoc library libraries).root

  method get_library_doc_type ~library : string =
    (List.assoc library libraries).doc_type

  method get_full_path ~library (rel_path : string) : string =
    let root = (self#get_library_root library) in
    (root ^"/"^rel_path)
    
  method open_doc ~library ~path : unit =
    let path = (self#get_full_path ~library path) in
    (Utilities.Sys.xopen path)
    
  method get_libraries () : string list =
    List.map fst libraries
    
  method add_library ~library ~root ~doc_type : unit =
    (if (library = "" || root = "") then
       raise InvalidLibrary);
    (if (List.mem_assoc library libraries) then
       raise LibraryExists);
    let lib = {name = library; root = root; doc_type = doc_type} in
    let libs = (List.rev ((library,lib) :: (List.rev libraries))) in
    let json = (libs_to_json version libs) in
    let _ = Json.to_file libconfig json in
    (prerr_endline ("Creating library: "^library^" -> "^root));
    (Sys.mkdir (store^"/"^library) 0o755);
    libraries <- libs

  method remove_library ~library : unit =
    (prerr_endline ("Removing: "^(store^"/"^library)));
    (Utilities.Sys.rmdir (store^"/"^library));
    let libs = List.remove_assoc library libraries in
    let json = (libs_to_json version libs) in
    let _ = Json.to_file libconfig json in
    libraries <- libs

  method rename_library ~library new_name : unit =
    (if (List.mem_assoc new_name libraries) then
       raise LibraryExists);
    let old_lib = (List.assoc library libraries) in
    (prerr_endline ("Renaming Library: "^library^" -> "^new_name^" ~> "^(Str.quote new_name)));
    (Utilities.Sys.move (store^"/"^library) (store^"/"^new_name));
    let libs = List.remove_assoc library libraries in
    let lib = {name = new_name; root = old_lib.root; doc_type = old_lib.doc_type} in
    let libs = (List.rev ((new_name,lib) :: (List.rev libs))) in
    let json = (libs_to_json version libs) in
    let _ = Json.to_file libconfig json in
    libraries <- libs


  (* TODO: Remove empty directories *)
  method check_library_integrity ~library : doc list =
    (prerr_endline ("Checking integrity of library: "^library));
    let docs = self#get_documents ~library in
    let library_root = self#get_library_root ~library in
    List.filter (fun doc ->
        let full_path = self#get_full_path ~library doc.path in
        if (not (Sys.file_exists full_path)) then
          ((prerr_endline ("Misplaced file: "^full_path));
           (prerr_endline ("Searching: "^store));
           let root_path = (self#get_library_root ~library) in
           match (Utilities.Sys.find_file doc.hash root_path) with
           | Some path ->
              let path = (Str.replace_first (Str.regexp (Str.quote ".json")) "" path) in
              prerr_endline ("Found new location: "^path);
              let rel_path = (get_rel_path ~library_root path) in
              let new_doc = (edit_document (Path rel_path) doc) in
              (self#add_document ~library new_doc);
              (self#remove_document ~library ~path:doc.path);
              false
           | None ->
              prerr_endline ("Could not find file: "^full_path^
                               "\n It was either placed in a different library or deleted!");
              true)
        else false)
      docs
end
  

  
