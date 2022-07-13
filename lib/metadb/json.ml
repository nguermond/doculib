(******************************************************************************)
(* Metadb                                                                     *)
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

include Yojson.Basic

      
exception ParsingFailure of string

let write_string (j : t) : string =
  to_string j

let pretty_to_string  (j : t) : string =
  pretty_to_string j


let from_string (s : string) : t =
  from_string s

let get (key : string) (json : t) : t option =
  match json with
  | `Assoc kv_lst ->
     (match (List.assoc_opt key kv_lst) with
      | Some v -> Some v
      | None -> None)
  | _ -> raise (ParsingFailure "Not an association list")

let to_list (json : t) : t list =
  match json with
  | `List j_lst -> j_lst
  | _ -> raise (ParsingFailure "Not a list")

let to_int (json : t) : int =
  match json with
  | `Int k -> k
  | _ -> let pp = pretty_to_string json in
         raise (ParsingFailure ("Not an int:"^pp))

let to_bool (json : t) : bool =
  match json with
  | `Bool b -> b
  | _ -> raise (ParsingFailure "Not a bool")
         
let to_string (json : t) : string =
  match json with
  | `String s -> s
  | _ -> raise (ParsingFailure "Not a string")
       
let raise_opt err (x : 'a option) : 'a =
  match x with
  | None
  | Some `Null ->  raise (ParsingFailure err)
  | Some x -> x

let default d (x : 'a option) : 'a =
  match x with
  | None 
  | Some `Null -> d
  | Some x -> x

let get_err (key : string) (json : t) : t =
  raise_opt ("Unbound key: `"^key^"`") (get key json)

let add_entry (k : string) (v : t) (json : t) : t =
  match json with
  | `Assoc kv_lst -> (`Assoc ((k,v)::kv_lst))
  | _ -> raise (ParsingFailure "Not an association list")

let remove_entry (k : string) (json : t) : t =
  match json with
  | `Assoc kv_lst -> (`Assoc (List.remove_assoc k kv_lst))
  | _ -> raise (ParsingFailure "Not an association list")

let to_file (path : Path.root) (json : t) : unit =
  to_file (Path.string_of_root path) json

let from_file (path : Path.root) : t =
  from_file (Path.string_of_root path)
