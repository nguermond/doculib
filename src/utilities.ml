
exception InternalError of string

module Sys =
  struct
    include Sys
    let xopen (str : string) : unit =
      (* Linux *)
      if (Sys.command ("xdg-open \""^str^"\" &")) > 0 then
        (* MacOS *)
        (if (Sys.command ("open \""^str^"\" &")) > 0 then
           (raise (InternalError (str ^ " could not be opened!"))))

    let hash (file : string) : string =
      (Digest.to_hex (Digest.file file))

      
    let rec get_files (path : string) ~(map:string -> 'a): 'a list =
      (if (Sys.is_directory path) then
         (List.flatten
            (List.map (fun name -> get_files (path^"/"^name) ~map)
               (Array.to_list (Sys.readdir path))))
       else [map path])

    let find_file (hash : string) (full_path : string) : string option =
      let hash = (Digest.from_hex hash) in
      List.find_map (fun path ->
          (if (Digest.equal (Digest.file path) hash) then
             (Some path) else None))
        (get_files full_path ~map:(fun x -> x))

    let rmdir (path : string) : unit =
      (if (Sys.command ("rm -r \""^path^"\"")) > 0 then
         (raise (InternalError ("Could not remove directory: "^path))))

    (* let mkdir (path : string) : unit =
     *   (if (Sys.command ("mkdir -p \""^path^"\"")) > 0 then
     *      (raise (InternalError ("Could not make directory: "^path)))) *)

    let make_dirs (root_path : string) (path : string) : unit =
      let rec make_dirs_ path dirs : unit =
        (if Sys.file_exists path then ()
         else (Sys.mkdir path 0o755));
        match dirs with
        | [] -> failwith "Not a full path"
        | [name] -> ()
        | dir::dirs -> make_dirs_ (path^"/"^dir) dirs
      in (make_dirs_ root_path (Str.split (Str.regexp "/") path))

    let move (path : string) (new_path : string) : unit =
      (if (Sys.command ("mv \""^path^"\" \""^new_path^"\"")) > 0 then
         (raise (InternalError ("Could not move directory: "^path^" -> "^new_path))))

  end

module List =
  struct
    include List
    let index (lst : 'a list) (x : 'a) : int option =
      let ind = ref None in
      (List.iteri (fun j y ->
           (if (x = y) && (!ind = None) then ind := (Some j))) lst);
      !ind

    let assoc_index (lst : ('a * 'b) list) (x : 'a) : int option =
      let ind = ref None in
      (List.iteri (fun j (y,_) ->
           (if (x = y) && (!ind = None) then ind := (Some j))) lst);
      !ind

  end
