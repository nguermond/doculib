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

exception CannotMerge
        
type t = {star : bool;
          title : string;
          authors : string list;
          doi : string;
          isbn : string;
          year : string;
          tags : string list;
          notes : string;
         }

type attribute =  Star of bool
                | Title of string
                | Authors of string list
                | Doi of string
                | Isbn of string
                | Year of string
                | Tags of string list
                | Notes of string

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
  | "notes" -> Notes value
  | _ -> failwith "Not a field"

let edit_document (field : attribute) (doc : t) : t =
  {star = (match field with Star v -> v | _ -> doc.star);
   title = (match field with Title v -> v | _ -> doc.title);
   authors = (match field with Authors v -> v | _ -> doc.authors);
   doi = (match field with Doi v -> v | _ -> doc.doi);
   isbn = (match field with Isbn v -> v | _ -> doc.isbn);
   year = (match field with Year v -> v | _ -> doc.year);
   tags = (match field with Tags v -> v | _ -> doc.tags);
   notes = (match field with Notes v -> v | _ -> doc.notes);
  }         
  
let pp_doc ppf (d : t) =
  let open Format in
  (fprintf ppf "{@\n%s%s%s%s%s%s%s%s}"
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
     (if d.notes = "" then ""
      else (sprintf "  Notes: %s@\n" d.notes))
  )

               

    
let to_json (doc : t) : Json.t =
  `Assoc [("star", `Bool doc.star);
          ("title", `String doc.title);
          ("authors", `List (List.map (fun x -> `String x) doc.authors));
          ("doi", `String doc.doi);
          ("isbn", `String doc.isbn);
          ("year", `String doc.year);
          ("tags", `List (List.map (fun x -> `String x) doc.tags));
          ("notes", `String doc.notes);
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
    notes = (to_string (raise_opt "notes" (get "notes" json)));
  }


let serialize_description ~library ~paths : string =
  let json =
    `Assoc [("library", `String library);
            ("paths", `List (List.map (fun x ->
                                 `String (Path.string_of_rel x)) paths))] in
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
   notes="";
  }

let to_string doc = (Format.asprintf "%a" pp_doc doc)

let merge_str str1 str2 =
  if str1 = "" then str2
  else if str2 = "" then str1
  else if str1 = str2 then str1
  else (raise (CannotMerge))

let merge_lst lst1 lst2 =
  if (lst1 = []) then lst2
  else if (lst2 = []) then lst1
  else if (lst1 = lst2) then lst1
  else (raise (CannotMerge))
                  
let merge doc1 doc2 =
  try
    Some
      {star = doc1.star || doc2.star;
       title = merge_str doc1.title doc2.title;
       authors = merge_lst doc1.authors doc2.authors;
       doi = merge_str doc1.doi doc2.doi;
       isbn = merge_str doc1.doi doc2.doi;
       year = merge_str doc1.year doc2.year;
       tags = merge_lst doc1.tags doc2.tags;
       notes = merge_str doc1.notes doc2.notes;
      }
  with CannotMerge -> Log.push ("Could not merge!");None
