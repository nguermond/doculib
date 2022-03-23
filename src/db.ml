open Lwt.Syntax
open Format

(* exception InitializationFailure of string *)
exception LibraryExists
exception InvalidLibrary
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


let get_rel_path ~library (path : string) : string =
  (Str.replace_first (Str.regexp (".json")) ""
     (Str.replace_first (Str.regexp (".*/"^library^"/")) "" path))

let json_to_libs (json : Json.t) : (string * library) list =
  (Json.to_list (Json.raise_opt "Could not find entry `libraries`"
                   (Json.get "libraries" json)))
  |> List.map (fun json ->
         let name = Json.to_string (Json.raise_opt "Could not find entry `name`" (Json.get "name" json)) in
         let path = Json.to_string (Json.raise_opt "Could not find entry `path`" (Json.get "path" json)) in
         let doc_type = Json.to_string (Json.raise_opt "Could not find entry `doc_type`" (Json.get "doc_type" json)) in
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
                         (`Assoc [("name",`String name);
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
    
  (* TODO: move to utilities *)
  method private make_dirs (path : string) : unit =
    let rec make_dirs_ path dirs : unit =
      (* prerr_endline ("Making dir: " ^ path); *)
      (if Sys.file_exists path then ()
       else (Sys.mkdir path 0o755));
      match dirs with
      | [] -> failwith "Not a full path"
      | [name] -> prerr_endline name; ()
      | dir::dirs -> make_dirs_ (path^"/"^dir) dirs
    in (make_dirs_ store (Str.split (Str.regexp "/") path))
     
(* Store document metadata as
 * store/library/path.json *)  
  method add_document ~library (doc : doc) : unit =
    let name = (store^"/"^library^"/"^doc.path^".json") in
    let json = doc_to_json doc in
    (self#make_dirs (library^"/"^doc.path));
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
    List.filter_map (fun path ->
        (self#import_file ~library path))
      paths

  method refresh_library ~library : doc list =
    let root = (self#get_library_root ~library) in
    let files = Utilities.Sys.get_files root in
    let paths = (List.map (get_rel_path ~library) files) in
    (self#import_files ~library paths)

  method get_documents ~library : doc list =
    List.map (fun path ->
        let path = get_rel_path ~library path in
        self#get_document ~library ~path)
      (Utilities.Sys.get_files (store^"/"^library))
  
  method private print_documents library : unit =
    let docs = self#get_documents library in
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
    let json = Yojson.Basic.from_file libconfig in
    let version = (get_lib_version ()) in
    let libs = (json_to_libs json) in
    let lib = {name = library; root = root; doc_type = doc_type} in
    let libs = (library,lib) :: libs in
    let json = (libs_to_json version libs) in
    let _ = Json.to_file libconfig json in
    (prerr_endline ("Creating library: "^library^" -> "^root));
    (Sys.mkdir (store^"/"^library) 0o755);
    libraries <- libs

  method remove_library ~library : unit =
    (* delete ~/.doculib/data/2.1/library *)
    (prerr_endline ("Removing: "^(store^"/"^library)));
    (Utilities.Sys.rmdir (store^"/"^library));
    (* remove entry in ~/.doculib/libraries.json *)
    let json = Yojson.Basic.from_file libconfig in
    let version = (get_lib_version ()) in
    let libs = (json_to_libs json) in
    let libs = List.remove_assoc library libs in
    let json = (libs_to_json version libs) in
    let _ = Json.to_file libconfig json in
    libraries <- libs
    
  method check_library_integrity ~library : doc list =
    (prerr_endline ("Checking integrity of library: "^library));
    let docs = self#get_documents ~library in
    List.filter (fun doc ->
        let full_path = self#get_full_path ~library doc.path in
        if (not (Sys.file_exists full_path)) then
          ((prerr_endline ("Misplaced file: "^full_path));
           (prerr_endline ("Searching: "^store));
           let root_path = (self#get_library_root ~library) in
           match (Utilities.Sys.find_file doc.hash root_path) with
           | Some path ->
              prerr_endline ("Found new location: "^path);
              let rel_path = (get_rel_path ~library path) in
              let new_doc = (edit_document (Path rel_path) doc) in
              (self#add_document ~library new_doc);
              (self#remove_document ~library ~path:doc.path);
              false
           | None ->
              prerr_endline ("Could not find file: "^full_path^"\n It was either placed in a different library or deleted!");
              true)
        else false)
      docs
end
  

  
