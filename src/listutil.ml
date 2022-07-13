
exception InternalError
      
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
  
let rec insert xs i x =
  match xs, i with
  | _, 0 -> x::xs
  | u::xs, i -> u::(insert xs (i - 1) x)
  | [], i -> raise (InternalError)

let rec rename_assoc x y xs =
  match xs with
  | (k,v)::xs when k = x -> (y,v)::xs
  | a::xs -> a::(rename_assoc x y xs)
  | [] -> raise (InternalError)
