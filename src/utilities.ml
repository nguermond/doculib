
module Sys =
  struct
    include Sys
    let xopen (str : string) : unit =
      (* Linux *)
      if (Sys.command ("xdg-open \""^str^"\"")) > 0 then
        (* MacOS *)
        (if (Sys.command ("open \""^str^"\"")) > 0 then
           prerr_endline (str ^ " could not be opened!"))

    let hash (file : string) : string =
      (Digest.to_hex (Digest.file file))
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
