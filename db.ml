open Lwt.Syntax
open Format

(* We store an absolute path to a library,
 * and paths are relative to this path. 
 * The path to the file is also its key.
*)

(* TODO: Allow multiple roots *)
let root = ref ""
let data = ref ""
         
let config () = Irmin_fs.config !data
                    
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


let add_document store (doc : doc) : unit Lwt.t =
  let info = Irmin_unix.info "Added %s" doc.path in
  Store.set_exn store [doc.path] doc ~info

let get_document path : doc =
  Lwt_main.run
    (let* repo = Store.Repo.v (config ()) in
     let* store = Store.master repo in
     Store.get store [path])

let set_document path (doc : doc) : unit =
  Lwt_main.run
    (let* repo = Store.Repo.v (config ()) in
     let* store = Store.master repo in
     let info = Irmin_unix.info "Update %s" path in
     Store.set_exn store [path]  doc ~info);
  ()

let remove_document path : unit =
  Lwt_main.run
    (let* repo = Store.Repo.v (config ()) in
     let* store = Store.master repo in
     let info = Irmin_unix.info "Removed %s" path in
     Store.remove store [path] ~info);
  ()

let import_file rel_path doc_type : doc option =
  Lwt_main.run
    (let* repo = Store.Repo.v (config ()) in
     let* store = Store.master repo in
     let info = Irmin_unix.info "Adding %s" rel_path in
     (* Check if key already exists! *)
     let* exists_opt = (Store.find store [rel_path]) in
     (match exists_opt with
      | Some _ -> prerr_endline ("Key already exists: "^rel_path);
                  Lwt.return None
      | None ->
         let doc = (make_doc_from_file rel_path doc_type) in
         let* _ = add_document store doc in
         Lwt.return (Some doc)))
            
let import_directory store path doc_type : (doc list) Lwt.t =
  let files = Array.to_list (Sys.readdir ((!root)^path)) in
  (Lwt_list.fold_left_s (fun docs name ->
       let key = (path ^ name) in
       (* Check if key already exists! *)
       let* exists_opt = (Store.find store [key]) in
       (match exists_opt with
        | Some _ -> prerr_endline ("Key already exists: "^key);
                    Lwt.return docs
        | None ->
           let doc = (make_doc_from_file key doc_type) in
           let* _ = add_document store doc in
           Lwt.return (doc::docs)))
     [] files)

let get_documents () : doc list =
  Lwt_main.run
    (let* repo = Store.Repo.v (config ()) in
     let* store = Store.master repo in
     let* kvs = Store.list store [] in
     (Lwt_list.fold_left_s (fun docs (key,tree) ->
          let* doc = Store.get store [key] in
          Lwt.return (doc::docs))
        [] kvs))

  
let print_documents () =
  let docs = get_documents () in
  List.iter (fun d -> printf "%a@\n" pp_doc d) docs
  

let import_documents path doc_type () : doc list =
  Lwt_main.run
    (let* repo = Store.Repo.v (config ()) in
     let* t = Store.master repo in
     let* docs = import_directory t path doc_type in
     Lwt.return docs)

let init () =
  (if (not (Sys.file_exists "doculib_config.json")) then
     failwith "No configuration file!");
  let json = Yojson.Basic.from_file "doculib_config.json" in
  let r = Json.to_string (Json.raise_opt "root not found" (Json.get "root" json)) in
  let d = Json.to_string (Json.raise_opt "data not found" (Json.get "data" json)) in
  prerr_endline ("Configuration:\n"^r^"\n"^d);
  root := r;
  data := d
