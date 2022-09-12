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
   
type t = {star : bool;
          title : string;
          authors : string list;
          doi : string;
          isbn : string;
          year : string;
          tags : string list;
          notes : string
         }

type attribute =  Star of bool
                | Title of string
                | Authors of string list
                | Doi of string
                | Isbn of string
                | Year of string
                | Tags of string list
                | Notes of string
  
val set_attribute : string -> string -> attribute
    
val pp_doc : Format.formatter -> t -> unit
val edit_document : attribute -> t -> t

val to_json : t -> Json.t
val from_json : Json.t -> t

val serialize_description : library:string -> paths:(Path.rel list) -> string
val deserialize_description : string -> (string * (Path.rel list))


val init : t


val to_string : t -> string

val merge : t -> t -> t option
val force_merge : t -> default:t -> t
