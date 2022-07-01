
exception OSError of string
exception InternalError of string

include Sys

let xopen str : unit =
  (* Linux *) 
  if (Sys.command ("xdg-open \""^str^"\" &")) > 0 then
    (* MacOS *)
    (if (Sys.command ("open \""^str^"\" &")) > 0 then
       (raise (OSError (str ^ " could not be opened!"))))
  
let open_file (path : Path.root) : unit =
  xopen (Path.to_string path)
  
let open_url (url : string) : unit =
  xopen url
  
(* Get files recursively *)
let rec get_files (path : Path.root) : Path.root Seq.t =
  (if (Sys.is_directory (Path.to_string path)) then
     (Seq.concat_map (fun name ->
          let name = Path.mk_name name in
          get_files (Path.merge_lst path [name]))
        (Array.to_seq (Sys.readdir (Path.to_string path))))
   else (Seq.return path))

(* Remove directory recursively *)
(* TODO: Rewrite this using Sys.remove *)
let rmdir (path : Path.root) : unit =
  (if (Sys.command ("rm -r \""^(Path.to_string path)^"\"")) > 0 then
     (raise (OSError ("Could not remove directory: "^(Path.to_string path)))))


let remove (path : Path.root) : unit =
  Sys.remove (Path.to_string path)
  
let make_dirp_rel ?(ignore_leaf=true) (root : Path.root) (path : Path.rel) : unit =
  let rec make_dirs_ root dirs : unit =
    (if Sys.file_exists (Path.to_string root) then ()
     else (Sys.mkdir (Path.to_string root) 0o755));
    match dirs with
    | [] -> ()
    | [name] -> (if ignore_leaf then () else
                   (make_dirs_ (Path.merge_lst root [name]) []))
    | dir::dirs -> make_dirs_ (Path.merge_lst root [dir]) dirs
  in (make_dirs_ root (Path.split path))

let make_dirp (root : Path.root) : unit =
  let (root,path) = Path.unroot root in
  make_dirp_rel root path

let make_dirp_leaf (root : Path.root) : unit =
  let (root,path) = Path.unroot root in
  make_dirp_rel ~ignore_leaf:false root path
  
let move (path : Path.root) (new_path : Path.root) : unit =
  (if (Sys.command ("mv \""^(Path.to_string path)^"\" \""
                    ^(Path.to_string new_path)^"\"")) > 0 then
     (raise (OSError ("Could not move: "^(Path.to_string path)
                      ^" -> "^(Path.to_string new_path)))))
    
let file_exists (path : Path.root) : bool =
  Sys.file_exists (Path.to_string path)

let empty_dir (path : Path.root) : bool =
  List.of_seq (get_files path) = []

let getenv_opt = Sys.getenv_opt
