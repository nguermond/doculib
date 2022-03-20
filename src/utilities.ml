
module Sys =
  struct
    include Sys
    let xopen (str : string) : unit =
      (* Linux *)
      let ret = Sys.command ("xdg-open \""^str^"\"") in
      if ret > 0 then
        (* MacOS *)
        let ret = (Sys.command ("open \""^str^"\"")) in
        if ret > 0 then
          prerr_endline (str ^ " could not be opened!")
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
