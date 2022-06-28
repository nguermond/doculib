
exception OSError of string
exception InternalError of string

include Sys

let xopen (path : Path.root) : unit =
  (* Linux *) 
  if (Sys.command ("xdg-open \""^(Path.to_string path)^"\" &")) > 0 then
    (* MacOS *)
    (if (Sys.command ("open \""^(Path.to_string path)^"\" &")) > 0 then
       (raise (OSError ((Path.to_string path) ^ " could not be opened!"))))

(* Get files recursively *)
let rec get_files_map (path : Path.root) (map : Path.root -> 'a) : 'a list =
  (if (Sys.is_directory (Path.to_string path)) then
     (List.flatten
        (List.map (fun name ->
             let name = Path.mk_name name in
             get_files_map (Path.merge_lst path [name]) map)
           (Array.to_list (Sys.readdir (Path.to_string path)))))
   else [map path])

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
  (get_files_map path (fun x -> x)) = []
