
exception IncorrectPathType of string

type root = string
type rel = string
type name = string
type path = name list



let debug = true
          
let root_type = (Str.regexp "/.*[^/]")
let rel_type = (Str.regexp "[^/].+[^/]")
let name_type = (Str.regexp "[^/]+")
let leaf_type = (Str.regexp "/[^/]+")
       
let mk_root (root : string) : root =
  if ((not debug) || Str.string_match root_type root 0) then root
  else raise (IncorrectPathType root)

let mk_rel (rel : string) : rel =
  if ((not debug) || Str.string_match rel_type rel 0) then rel
  else raise (IncorrectPathType rel)

let mk_name (name : string) : name =
  if ((not debug) || Str.string_match name_type name 0) then name
  else raise (IncorrectPathType name)
  
let mk_path (path : string list) : path =
  if debug then (List.map mk_name path)
  else path

let unroot (root : root) : root * rel =
  (mk_root "/", (Str.replace_first (Str.regexp "/") "" root))

(* TODO: rename these to 'string_of_*' *)
let to_string (root : root) : string = root
let rel_to_string (rel : rel) : string = rel
let string_of_name (name : name) : string = name
                                     
let merge_lst (root : root) (path : path) : root =
  String.concat "/" (root :: path)

let merge (root : root) (path : rel) : root =
  merge_lst root [path]

let split (path : rel) : name list =
  let names = String.split_on_char '/' path in
  if debug then (List.map mk_name names)
  else names
    
let remove_file_ext_rel (ext : string) (path : rel) : rel =
  (Str.replace_first (Str.regexp ((Str.quote ("."^ext))^"$")) "" path)
    
let remove_file_ext (ext : string) (root : root) : root =
  remove_file_ext_rel ext root

let strip_root (root : root) (path : root) : rel =
  (Str.replace_first
     (Str.regexp ((Str.quote root)^"/")) "" path)

let drop_leaf (root : root) : root =
  (Str.replace_first leaf_type "" root)

let get_leaf (path : rel) : name =
  let names = (split path) in
  List.nth names ((List.length names) - 1)
