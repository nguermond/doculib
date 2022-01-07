open Lwt.Syntax
open Format

exception InitializationFailure of string
exception LibraryExists
                                 
(* We store an absolute path to each library, 
 * and a path to a file is relative to this path. 
 * The path to the file is also its key. 
 * The purpose of having a path for every library is that
 * an entire library can be relocated without having to modify entries. *)
let home = (Sys.getenv "HOME")
let data = home^"/.doculib/data"
let config = Irmin_fs.config data

(* The database is versioned, to prevent data loss upon upgrade. *)
let current_branch = "1.0"
let libconfig = home^"/.doculib/libraries.json"
let libraries : ((string * (string * string)) list) ref = ref []

(* Example keys for branch "1.0" look like:
    Articles/path/to/article1.pdf
    Articles/article2.djvu
    Textbooks/path/to/textbook1.pdf
    Textbooks/pathto/textbook2.pdf
    EBooks/long/path/to/ebook1.pdf 
*)
                    
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

type attribute =
  Star of bool
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
  (fprintf ppf "%s%s%s%s%s%s%s%s%s"
     (if d.star = false then ""
      else (sprintf "Starred"))
     (if d.title = "" then ""
      else (sprintf "Title: %s@\n" d.title))
     (if d.authors = [] then ""
      else (sprintf "Author(s): %s@\n" (String.concat "; " d.authors)))
     (if d.doi = "" then ""
      else (sprintf "DOI: %s@\n" d.doi))
     (if d.isbn = "" then ""
      else (sprintf "ISBN: %s@\n" d.isbn))
     (if d.year = "" then ""
      else (sprintf "Year: %s@\n" d.year))
     (if d.tags = [] then ""
      else (sprintf "Tags: %s@\n" (String.concat "; " d.tags)))
     (sprintf "Path: %s@\n" d.path)
     (sprintf "Document Type: %s@\n" d.doc_type))

module Doc = struct
  type t = doc

  let t =
    let open Irmin.Type in
    record "doc" (fun star title authors doi isbn year tags path doc_type ->
        {star; title; authors; doi; isbn; year; tags; path; doc_type})
    |+ field "star" bool (fun t -> t.star)
    |+ field "title" string (fun t -> t.title)
    |+ field "authors" (list string) (fun t -> t.authors)
    |+ field "doi" string (fun t -> t.doi)
    |+ field "isbn" string (fun t -> t.isbn)
    |+ field "year" string (fun t -> t.year)
    |+ field "tags" (list string) (fun t -> t.tags)
    |+ field "path" string (fun t -> t.path)
    |+ field "doc_type" string (fun t -> t.doc_type)
    |> sealr

  let merge = Irmin.Merge.(option (idempotent t))
end

module Store = Irmin_unix.FS.KV(Doc)


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


let add_document store library (doc : doc) : unit Lwt.t =
  let info = Irmin_unix.info "Add %s/%s" library doc.path in
  Store.set_exn store [library; doc.path] doc ~info
  
let get_document ~library ~path : doc =
  Lwt_main.run
    (let* repo = Store.Repo.v config in
     let* store = Store.of_branch repo current_branch in
     Store.get store [library; path])

let set_document ~library ~path doc : unit =
  Lwt_main.run
    (let* repo = Store.Repo.v config in
     let* store = Store.of_branch repo current_branch in
     let info = Irmin_unix.info "Set %s/%s" library path in
     Store.set_exn store [library; path] doc ~info);
  ()

let remove_document ~library ~path : unit =
  Lwt_main.run
    (let* repo = Store.Repo.v config in
     let* store = Store.of_branch repo current_branch in
     let info = Irmin_unix.info "Remove %s/%s" library path in
     Store.remove store [library; path] ~info);
  ()

let get_rel_path ~library (path : string) : string =
  Str.replace_first (Str.regexp (library^"/")) "" path
  
let import_file ~library ~doc_type path : doc option =
  Lwt_main.run
    (let* repo = Store.Repo.v config in
     let* store = Store.of_branch repo current_branch in
     let info = Irmin_unix.info "Import %s/%s" library path in
     let* exists_opt = (Store.find store [library; path]) in
     match exists_opt with
     | Some _ ->
        prerr_endline ("Key already exists: "^library^"/"^path);
        Lwt.return None
     | None ->
        let doc = (make_doc_from_file path doc_type) in
        let* _ = add_document store library doc in
        Lwt.return (Some doc))

let import_files ~library ~doc_type (paths : string list) : doc list =
  List.filter_map (import_file ~library ~doc_type) paths
  
let get_documents ~library : doc list =
  Lwt_main.run
    (let* repo = Store.Repo.v config in
     let* store = Store.of_branch repo current_branch in
     let* kvs = Store.list store [library] in
     (Lwt_list.fold_left_s (fun docs (key,tree) ->
          let* doc = Store.get store [key] in
          Lwt.return (doc::docs))
        [] kvs))
  
let print_documents library : unit =
  let docs = get_documents library in
  List.iter (fun d -> printf "%a@\n" pp_doc d) docs

let get_library_root ~library : string =
  fst (List.assoc library !libraries)

let get_library_doc_type ~library : string =
  snd (List.assoc library !libraries)
  
let get_libraries () : string list =
  List.map fst !libraries

let json_to_libs (json : Json.t) : (string * (string * string)) list =
  (Json.raise_opt "Could not find entry `libraries`"
     (Json.get "libraries" json))
  |> Json.to_list
  |> List.map (fun json ->
         let name = Json.to_string (Json.raise_opt "Could not find entry `name`" (Json.get "name" json)) in
         let path = Json.to_string (Json.raise_opt "Could not find entry `path`" (Json.get "path" json)) in
         let doc_type = Json.to_string (Json.raise_opt "Could not find entry `doc_type`" (Json.get "doc_type" json)) in
         (name,(path,doc_type)))

let libs_to_json (libs : (string * (string * string)) list) : Json.t =
  let libs = (`List (List.map (fun (name,(path,doc_type)) ->
                         (`Assoc [("name",`String name);
                                  ("path", `String path);
                                  ("doc_type", `String doc_type)]))
                       libs)) in
  (`Assoc [("libraries", libs)])
  
let add_library ~library ~root ~doc_type : unit =
  (if (List.mem_assoc library !libraries) then
     raise LibraryExists
   else
     let json = Yojson.Basic.from_file libconfig in
     let libs = (json_to_libs json) in
     let libs = (library,(root,doc_type)) :: libs in
     let json = (libs_to_json libs) in
     let _ = Json.to_file libconfig json in
     libraries := libs)
          
let init () : unit =
  (if (not (Sys.file_exists libconfig))
   then raise (InitializationFailure
                 ("Configuration file `"^libconfig^"` is missing!")));
  let json = Yojson.Basic.from_file libconfig in
  let libs = (json_to_libs json) in
  (List.iter (fun (lib,(path,doc_type)) -> prerr_endline (path^"/"^lib)) libs);
  libraries := libs


let get_full_path ~library (rel_path : string) : string =
  let root = (get_library_root library) in
  let path = (root ^"/"^rel_path) in
  prerr_endline ("Full path: "^path);
  path

(* let get_rel_path (path : string) : string =
 *   (String.sub path (String.length !Db.root)
 *      ((String.length path) - (String.length !Db.root))) *)
  
(* let get_doc_name (path : string) : string =
 *   let s_path = (String.split_on_char '/' path) in
 *   let name = (List.nth s_path ((List.length s_path) - 1)) in
 *   let names = (String.split_on_char '.' name) in
 *   let tmp = (List.rev (List.tl (List.rev names))) in
 *   (String.concat " " tmp) *)
  
let open_doc ~library ~path : unit =
  let root = (get_library_root library) in
  let path = (root^"/"^path) in
  let ret = Sys.command ("xdg-open \""^path^"\"") in
  (if ret > 0 then
     prerr_endline (path ^ " could not be opened!"))
