
exception InternalError of string

module Sys =
  struct
    include Sys
    let xopen (str : string) : unit =
      (* Linux *)
      if (Sys.command ("xdg-open \""^str^"\"")) > 0 then
        (* MacOS *)
        (if (Sys.command ("open \""^str^"\"")) > 0 then
           (raise (InternalError (str ^ " could not be opened!"))))

    let hash (file : string) : string =
      (Digest.to_hex (Digest.file file))

    let find_file (hash : string) (full_path : string) : string option =
      let hash = (Digest.from_hex hash) in
      let rec find_file_ (full_path : string) =
        List.find_map (fun path ->
            let full_path = (full_path^"/"^path) in 
            (if (Sys.is_directory full_path) then
               (find_file_ full_path)
             else
               (if (Digest.equal (Digest.file full_path) hash) then
                  (Some full_path) else None)))
          (Array.to_list (Sys.readdir full_path))
      in (find_file_ full_path)

    let rmdir (str : string) : unit =
      if (Sys.command ("rm -r \""^str^"\"")) > 0 then
        (raise (InternalError ("Could not remove directory: "^str)));
      
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
