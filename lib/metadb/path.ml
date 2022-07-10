
exception InvalidRootType of string
exception InvalidRelType of string
exception InvalidNameType of string

type root = string
type rel = string
type name = string

(* TODO: Look at the Fpath library & Fileutils *)


let debug = ref true

let set_debug (d : bool) : unit =
  debug := d
          
(* TODO: Maybe we should distinguish between files and directories? *)
let root_type = (Str.regexp "\\(/\\|\\(/[^/]+\\)*\\)$")
let rel_type = (Str.regexp "[^/]+\\(/[^/]+\\)*$")
let name_type = (Str.regexp "[^/]+$")
let leaf_type = (Str.regexp "/[^/]+$")
       
let mk_root (root : string) : root =
  if ((not !debug) || Str.string_match root_type root 0) then root
  else raise (InvalidRootType root)

let mk_rel (rel : string) : rel =
  if ((not !debug) || Str.string_match rel_type rel 0) then rel
  else raise (InvalidRelType rel)

let mk_name (name : string) : name =
  if ((not !debug) || Str.string_match name_type name 0) then name
  else raise (InvalidNameType name)
  
let mk_path (path : string list) : name list =
  if !debug then (List.map mk_name path)
  else path

let unroot (root : root) : root * rel =
  (mk_root "/", (Str.replace_first (Str.regexp "/") "" root))

let string_of_root (root : root) : string = root
let string_of_rel (rel : rel) : string = rel
let string_of_name (name : name) : string = name
                                     
let merge_lst (root : root) (path : name list) : root =
  String.concat "/" (root :: path)

let merge (root : root) (path : rel) : root =
  merge_lst root [path]

let split (path : rel) : name list =
  let names = String.split_on_char '/' path in
  if !debug then (List.map mk_name names)
  else names

let add_file_ext (ext : string) (root : root) : root =
  (root ^ "." ^ ext)
  
let remove_file_ext_rel (ext : string) (path : rel) : rel =
  (Str.replace_first (Str.regexp ((Str.quote ("."^ext))^"$")) "" path)
    
let remove_file_ext (ext : string) (root : root) : root =
  remove_file_ext_rel ext root

let strip_root (root : root) (path : root) : rel =
  if root = "/" then snd (unroot path) else
    (Str.replace_first
       (Str.regexp ((Str.quote root)^"/")) "" path)

let drop_leaf (root : root) : root =
  (Str.replace_first leaf_type "" root)

  
let get_leaf_rel (path : rel) : name =
  let names = (split path) in
  List.nth names ((List.length names) - 1)

let get_leaf (root : root) : name =
  let path = snd (unroot root) in
  get_leaf_rel path
  
let hidden (path : root) : bool =
  let leaf = get_leaf (strip_root "/" path) in
  leaf.[0] = '.'
