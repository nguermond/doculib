
exception OSError of string
exception InternalError of string
exception NotADirectory of Path.root

include Sys
      
let xopen str : unit =
  (* Linux *) 
  if (Sys.command ("xdg-open \""^str^"\" &")) > 0 then
    (* MacOS *)
    (if (Sys.command ("open \""^str^"\" &")) > 0 then
       (raise (OSError (str ^ " could not be opened!"))))
  
let open_file (path : Path.root) : unit =
  xopen (Path.string_of_root path)
  
let open_url (url : string) : unit =
  xopen url
  
(* Get files recursively *)
let get_files ?(hidden=false) (path : Path.root) : Path.root Seq.t =
  if (not (Sys.is_directory (Path.string_of_root path))) then
    raise (NotADirectory path);
  let rec get_files_ path =
    (if (not hidden) && (Path.hidden path) then (Seq.empty) else
       (if (Sys.is_directory (Path.string_of_root path)) then
          (Seq.concat_map (fun name ->
               let name = Path.mk_name name in
               get_files_ (Path.merge_lst path [name]))
             (Array.to_seq (Sys.readdir (Path.string_of_root path))))
        else (Seq.return path)))
  in (get_files_ path)

(* Remove directory recursively *)
let rmdir (path : Path.root) : unit =
  if (not (Sys.is_directory (Path.string_of_root path))) then
    raise (NotADirectory path);
  let rec rmdir_ path =
    (if (Sys.is_directory (Path.string_of_root path)) then
       ((Seq.iter (fun name ->
             (rmdir_ (Path.merge_lst path [Path.mk_name name])))
           (Array.to_seq (Sys.readdir (Path.string_of_root path))));
        Sys.rmdir (Path.string_of_root path))
     else
       (Sys.remove (Path.string_of_root path)))
  in (rmdir_ path)

let remove (path : Path.root) : unit =
  Sys.remove (Path.string_of_root path)
  
let make_dirp_rel ?(ignore_leaf=true) (root : Path.root) (path : Path.rel) : unit =
  let rec make_dirs_ root dirs : unit =
    (if Sys.file_exists (Path.string_of_root root) then ()
     else (Sys.mkdir (Path.string_of_root root) 0o755));
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
  FileUtil.mv (Path.string_of_root path) (Path.string_of_root new_path)
  
(* TODO: Rewrite this using POSIX syscalls (ie. using rename) *)
let move (path : Path.root) (new_path : Path.root) : unit =
  (if (Sys.command ("mv \""^(Path.string_of_root path)^"\" \""
                    ^(Path.string_of_root new_path)^"\"")) > 0 then
     (raise (OSError ("Could not move: "^(Path.string_of_root path)
                      ^" -> "^(Path.string_of_root new_path)))))
    
let file_exists (path : Path.root) : bool =
  Sys.file_exists (Path.string_of_root path)

let empty_dir (path : Path.root) : bool =
  List.of_seq (get_files path) = []

let getenv_opt = Sys.getenv_opt
