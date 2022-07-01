open Format
   
type t = {star : bool;
          title : string;
          authors : string list;
          doi : string;
          isbn : string;
          year : string;
          tags : string list;
          (* notes : string *)
         }

type attribute =  Star of bool
                | Title of string
                | Authors of string list
                | Doi of string
                | Isbn of string
                | Year of string
                | Tags of string list
                (* | Path of string
                 * | DocType of string
                 * | Hash of string *)
                           
let set_attribute (field : string) (value : string) : attribute =
  match field with
  | "star" -> Star (match value with
                      "true" -> true
                    | "false" -> false
                    | _ -> failwith "Cannot set Star attribute")
  | "title" -> Title value
  | "authors" -> Authors (Str.split (Str.regexp "; +") value)
  | "year" -> Year value
  | "doi" -> Doi value
  | "isbn" -> Isbn value
  | "tags" -> Tags (Str.split (Str.regexp "; +") value)
  (* | "path" -> Path value
   * | "doc_type" -> DocType value
   * | "hash" -> Hash value *)
  | _ -> failwith "Not a field"

let edit_document (field : attribute) (doc : t) : t =
  {star = (match field with Star v -> v | _ -> doc.star);
   title = (match field with Title v -> v | _ -> doc.title);
   authors = (match field with Authors v -> v | _ -> doc.authors);
   doi = (match field with Doi v -> v | _ -> doc.doi);
   isbn = (match field with Isbn v -> v | _ -> doc.isbn);
   year = (match field with Year v -> v | _ -> doc.year);
   tags = (match field with Tags v -> v | _ -> doc.tags);
   (* path = (match field with Path v -> v | _ -> doc.path); *)
   (* doc_type = (match field with DocType v -> v | _ -> doc.doc_type);
    * hash = (match field with Hash v -> v | _ -> doc.hash); *)
  }         
  
let pp_doc ppf (d : t) =
  (fprintf ppf "{@\n%s%s%s%s%s%s%s}"
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
     (* (sprintf "  Path: %s@\n" d.path)
      * (sprintf "  Document Type: %s@\n" d.doc_type)
      * (sprintf "  Hash: %s@\n" d.hash) *)
  )

               

    
let to_json (doc : t) : Json.t =
  `Assoc [("star", `Bool doc.star);
          ("title", `String doc.title);
          ("authors", `List (List.map (fun x -> `String x) doc.authors));
          ("doi", `String doc.doi);
          ("isbn", `String doc.isbn);
          ("year", `String doc.year);
          ("tags", `List (List.map (fun x -> `String x) doc.tags));
          (* ("doc_type", `String doc.doc_type);
           * ("hash", `String doc.hash); *)
    ]

let from_json (json : Json.t) : t =
  let open Json in
  { star = (to_bool (raise_opt "star" (get "star" json)));
    title = (to_string (raise_opt "title" (get "title" json)));
    authors = (List.map to_string
                 (to_list (raise_opt "authors" (get "authors" json))));
    doi = (to_string (raise_opt "doi" (get "doi" json)));
    isbn = (to_string (raise_opt "isbn" (get "isbn" json)));
    year = (to_string (raise_opt "year" (get "year" json)));
    tags = (List.map to_string
              (to_list (raise_opt "tags" (get "tags" json))));
    (* path = path;
     * doc_type = (to_string (raise_opt "doc_type" (get "doc_type" json)));
     * hash = (to_string (default (`String "hash") (get "hash" json))); *)
  }


let serialize_description ~library ~paths : string =
  let json =
    `Assoc [("library", `String library);
            ("paths", `List (List.map (fun x ->
                                 `String (Path.rel_to_string x)) paths))] in
  (Json.write_string json)

let deserialize_description (str : string) : (string * (Path.rel list)) =
  let json = (Json.from_string str) in
  let library = (Json.to_string (Json.raise_opt "library" (Json.get "library" json))) in
  let paths = (List.map (fun x -> Path.mk_rel @@ Json.to_string x)
                (Json.to_list (Json.raise_opt "paths" (Json.get "paths" json)))) in
  (library,paths)


let init : t =
  {star=false;
   title="";
   authors=[];
   doi="";
   isbn="";
   year="";
   tags=[];
  }

let to_string doc = (Format.asprintf "%a" pp_doc doc)
