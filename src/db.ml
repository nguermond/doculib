open Lwt.Syntax
open Format

(* exception InitializationFailure of string *)
exception LibraryExists
                                 
(* We store an absolute path to each library, 
 * and a path to a file is relative to this path. 
 * The path to the file is also its key. 
 * The purpose of having a path for every library is that
 * an entire library can be relocated without having to modify entries. *)

(* The database is versioned, to prevent data loss upon upgrade. *)
let branches = ["2.1"; "2.0"]
let current_branch = "2.0"

let configdir : string = (Sys.getenv "HOME")^"/.doculib"
let datadir : string = configdir^"/data"
let libconfig : string = configdir^"/libraries.json"


type doc = {star : bool;
            title : string;
            authors : string list;
            doi : string;
            isbn : string;
            year : string;
            tags : string list;
            path : string;
            doc_type : string;
           }

type attribute =  Star of bool
                | Title of string
                | Authors of string list
                | Doi of string
                | Isbn of string
                | Year of string
                | Tags of string list
                | Path of string
                | DocType of string
                           
let set_attribute (field : string) (value : string) : attribute =
  match field with
  | "star" -> Star (match value with "true" -> true | "false" -> false | _ -> failwith "Cannot set Star attribute")
  | "title" -> Title value
  | "authors" -> Authors (Str.split (Str.regexp "; +") value)
  | "year" -> Year value
  | "doi" -> Doi value
  | "isbn" -> Isbn value
  | "tags" -> Tags (Str.split (Str.regexp "; +") value)
  | "path" -> Path value
  | "doc_type" -> DocType value
  | _ -> failwith "Not a field"

let edit_document (field : attribute) (doc : doc) : doc =
  {star = (match field with Star v -> v | _ -> doc.star);
   title = (match field with Title v -> v | _ -> doc.title);
   authors = (match field with Authors v -> v | _ -> doc.authors);
   doi = (match field with Doi v -> v | _ -> doc.doi);
   isbn = (match field with Isbn v -> v | _ -> doc.isbn);
   year = (match field with Year v -> v | _ -> doc.year);
   tags = (match field with Tags v -> v | _ -> doc.tags);
   path = (match field with Path v -> v | _ -> doc.path);
   doc_type = (match field with DocType v -> v | _ -> doc.doc_type)
  }         
  
let pp_doc ppf (d : doc) =
  (fprintf ppf "{@\n%s%s%s%s%s%s%s%s%s}"
     (if d.star = false then ""
      else (asprintf "  Starred@\n"))
     (if d.title = "" then ""
      else (sprintf "  Title: %s@\n" d.title))
     (if d.authors = [] then ""
      else (sprintf "  Author(s): %s@\n" (String.concat "; " d.authors)))
     (if d.doi = "" then ""
      else (sprintf "  DOI: %s@\n" d.doi))
     (if d.isbn = "" then ""
      else (sprintf "  ISBN: %s@\n" d.isbn))
     (if d.year = "" then ""
      else (sprintf "  Year: %s@\n" d.year))
     (if d.tags = [] then ""
      else (sprintf "  Tags: %s@\n" (String.concat "; " d.tags)))
     (sprintf "  Path: %s@\n" d.path)
     (sprintf "  Document Type: %s@\n" d.doc_type))

               
let make_doc_from_file path doc_type : doc =
  {star=false;
   title="";
   authors=[];
   doi="";
   isbn="";
   year="";
   tags=[];
   path=path;
   doc_type=doc_type
  }

let doc_to_json (doc : doc) : Json.t =
  `Assoc [("star", `Bool doc.star);
          ("title", `String doc.title);
          ("authors", `List (List.map (fun x -> `String x) doc.authors));
          ("doi", `String doc.doi);
          ("isbn", `String doc.isbn);
          ("year", `String doc.year);
          ("tags", `List (List.map (fun x -> `String x) doc.tags));
          ("doc_type", `String doc.doc_type)]

let json_to_doc path (json : Json.t) : doc =
  let open Json in
  { star = (to_bool (raise_opt "" (get "star" json)));
    title = (to_string (raise_opt "" (get "title" json)));
    authors = (List.map to_string
                 (to_list (raise_opt "" (get "authors" json))));
    doi = (to_string (raise_opt "" (get "doi" json)));
    isbn = (to_string (raise_opt "" (get "isbn" json)));
    year = (to_string (raise_opt "" (get "year" json)));
    tags = (List.map to_string
              (to_list (raise_opt "" (get "tags" json))));
    path = path;
    doc_type = (to_string (raise_opt "" (get "doc_type" json)))
  }


let get_rel_path ~library (path : string) : string =
  (Str.replace_first (Str.regexp (".json")) ""
     (Str.replace_first (Str.regexp (".*/"^library^"/")) "" path))

let json_to_libs (json : Json.t) : (string * (string * string)) list =
  (Json.raise_opt "Could not find entry `libraries`"
     (Json.get "libraries" json))
  |> Json.to_list
  |> List.map (fun json ->
         let name = Json.to_string (Json.raise_opt "Could not find entry `name`" (Json.get "name" json)) in
         let path = Json.to_string (Json.raise_opt "Could not find entry `path`" (Json.get "path" json)) in
         let doc_type = Json.to_string (Json.raise_opt "Could not find entry `doc_type`" (Json.get "doc_type" json)) in
         (name,(path,doc_type)))

let get_lib_version (json : Json.t) : string =
  Json.to_string
    (Json.raise_opt "Could not find entry `version`"
       (Json.get "version" json))

let libs_to_json (version : string) (libs : (string * (string * string)) list) : Json.t =
  let libs = (`List (List.map (fun (name,(path,doc_type)) ->
                         (`Assoc [("name",`String name);
                                  ("path", `String path);
                                  ("doc_type", `String doc_type)]))
                       libs)) in
  (`Assoc [("version",`String version);
           ("libraries", libs)])

   
class db branch =
object (self)
  val store : string = datadir^"/"^branch
  val mutable libraries : ((string * (string * string)) list) =
    let store = (datadir^"/"^branch) in
    (if (not (Sys.file_exists store)) then
       (prerr_endline "store directory does not exist: creating...";
        Sys.mkdir store 0o755));
    let json = Yojson.Basic.from_file libconfig in
    (json_to_libs json)
    
  (* TODO: move to utilities *)
  method private make_dirs (dirs : string list) : unit =
    let rec make_dirs_ path dirs : unit =
      (* prerr_endline ("Making dir: " ^ path); *)
      (if Sys.file_exists path then ()
       else (Sys.mkdir path 0o755));
      match dirs with
      | [] -> failwith "Not a full path"
      | [name] -> prerr_endline name; ()
      | dir::dirs -> make_dirs_ (path^"/"^dir) dirs
    in (make_dirs_ store dirs)
     

(* Store document metadata as
 * store/library/path.json *)  
  method private add_document ~library (doc : doc) : unit =
    let name = (store^"/"^library^"/"^doc.path^".json") in
    (* (print_endline ("adding "^name)); *)
    let json = doc_to_json doc in
    let dirs = (Str.split (Str.regexp "/") (library^"/"^doc.path)) in
    (self#make_dirs dirs);
    (Json.to_file name json)
  
  method get_document ~library ~path : doc =
    let name = (store^"/"^library^"/"^path^".json") in
    (* print_endline ("getting "^name); *)
    let json = (Json.from_file name) in
    (json_to_doc path json)
  
  method set_document ~library ~path doc : unit =
    let name = (store^"/"^library^"/"^path^".json") in
    (* (print_endline ("setting "^name)); *)
    let json = doc_to_json doc in
    (Json.to_file name json)
  
  method remove_document ~library ~path : unit =
    let name = (store^"/"^library^"/"^path^".json") in
    (* print_endline ("removing "^ name); *)
    Sys.remove name
 
  method private import_file ~library ~doc_type path : doc option =
    let name = (store^"/"^library^"/"^path^".json") in
    (* prerr_endline ("importing file: "^name); *)
    (if (Sys.file_exists name) then
       ((* prerr_endline ("Key already exists: "^library^"/"^path); *)
        None)
     else
       (let doc = (make_doc_from_file path doc_type) in
        let _ = self#add_document ~library doc in
        (Some doc)))

  method import_files ~library ~doc_type (paths : string list) : doc list =
    let n = (List.length paths) in
    let i = ref 0 in
    (prerr_endline "Importing files...");
    List.filter_map (fun path ->
        let load = ((float_of_int !i) /. (float_of_int n)) in
        prerr_endline (string_of_float load);
        i := !i+1;
        (self#import_file ~library ~doc_type path))
      paths

  method get_documents ~library : doc list =
    let rec get_files (full_path : string) : doc list =
      (if not (Sys.is_directory full_path) then
         (let path = get_rel_path ~library full_path in
          [self#get_document ~library ~path])
       else
         (List.flatten
            (List.map (fun name -> get_files (full_path^"/"^name))
               (Array.to_list (Sys.readdir full_path)))))
    in (get_files (store^"/"^library))
  
  method private print_documents library : unit =
    let docs = self#get_documents library in
    List.iter (fun d -> printf "%a@\n" pp_doc d) docs
    
  method get_library_root ~library : string =
    fst (List.assoc library libraries)

  method get_library_doc_type ~library : string =
    snd (List.assoc library libraries)

  method get_full_path ~library (rel_path : string) : string =
    let root = (self#get_library_root library) in
    (root ^"/"^rel_path)
    
  method open_doc ~library ~path : unit =
    let path = (self#get_full_path ~library path) in
    (Utilities.Sys.xopen path)
  
  method get_libraries () : string list =
    List.map fst libraries
  
  method add_library ~library ~root ~doc_type : unit =
    (if (List.mem_assoc library libraries) then
       raise LibraryExists
     else
       let json = Yojson.Basic.from_file libconfig in
       let version = (get_lib_version json) in
       let libs = (json_to_libs json) in
       let libs = (library,(root,doc_type)) :: libs in
       let json = (libs_to_json version libs) in
       let _ = Json.to_file libconfig json in
       (prerr_endline "creating library...";
        Sys.mkdir (store^"/"^library) 0o755);
       libraries <- libs)
end
  
let update_db (version : string) (json : Json.t) : unit =
  (printf "Updating version %s to version %s@\n" version current_branch);
  failwith "NYI"
  (* 1. update libraries.json
     2. migrate each library
   *)

  
let init () : unit =
  (if (not (Sys.file_exists configdir)) then
     (prerr_endline "configuration directory does not exist: creating...";
      Sys.mkdir configdir 0o755));
  (if (not (Sys.file_exists datadir)) then
     (prerr_endline "data directory does not exist: creating...";
      Sys.mkdir datadir 0o755));
  (if (not (Sys.file_exists libconfig)) then
     (prerr_endline "configuration file does not exist: creating...";
      let json = (libs_to_json current_branch []) in
      Json.to_file libconfig json));
  let json = Yojson.Basic.from_file libconfig in
  let version = (get_lib_version json) in
  (if version <> current_branch then
     (update_db version json))

  
