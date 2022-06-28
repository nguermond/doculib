
exception InternalError of string

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
