
include Yojson.Basic

exception ParsingFailure of string

let get (key : string) (json : t) : t option =
  match json with
  | `Assoc kv_lst ->
     (match (List.assoc_opt key kv_lst) with
      | Some v -> Some v
      | None -> None)
  | _ -> None

let to_list (json : t) : t list =
  match json with
  | `List j_lst -> j_lst
  | _ -> raise (ParsingFailure "Not a list")

let to_int (json : t) : int =
  match json with
  | `Int k -> k
  | _ -> let pp = pretty_to_string json in
         raise (ParsingFailure ("Not an int:"^pp))

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
