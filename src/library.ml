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
exception InvalidDocType

open Metadb

(* The database is versioned, to prevent data loss upon upgrade. *)
let branches = ["3.0"; "2.1"; "2.0"]
let current_branch = "3.0"

type doc_type = [`Book | `Article]

let string_of_doc_type = function
  | `Book -> "book"
  | `Article -> "article"
               
let doc_type_of_string = function
  | "book" -> `Book
  | "article" -> `Article
  | _ -> raise (InvalidDocType)

type t = {
    version : string;
    doc_type : doc_type;
  }
let to_json (lib : t) : Json.t =
  (`Assoc [("version",`String lib.version);
           ("doc_type", `String (string_of_doc_type lib.doc_type))
  ])
let from_json (j : Json.t) : t =
  let v = (Json.to_string (Json.get_err "version" j)) in
  let dtype = (doc_type_of_string (Json.to_string (Json.get_err "doc_type" j)))
  in {version = v ; doc_type = dtype}

let get_doc_type (ld : t) : doc_type =
  ld.doc_type

let get_version (ld : t) : string =
  ld.version

let make (dtype : doc_type) : t =
  {version = current_branch;
   doc_type = dtype}
