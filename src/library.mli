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
   
type doc_type = [`Book | `Article]

(* TODO : these should be private *)
val string_of_doc_type : doc_type -> string
val doc_type_of_string : string -> doc_type
              
type t = {
    version : string;
    doc_type : doc_type;
  }

val get_doc_type : t -> doc_type

val get_version : t -> string

val to_json : t -> Json.t
  
val from_json : Json.t -> t
  
val make : doc_type -> t
