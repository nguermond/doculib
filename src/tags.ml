open Graph

module Tag =
  struct
      type t = string
      let compare = String.compare
      let equal = String.equal
      let hash x = Int64.to_int(String.get_int64_ne x 0)
  end
         
module G = Imperative.Digraph.ConcreteBidirectional(Tag)
         
let g = G.create()

      
let add_tags (tags : Tag.t list) =
  List.iter (fun t ->
      let v = G.V.create t in
      G.add_vertex g v)
    tags

let add_synonyms (syn : (Tag.t * Tag.t) list) =
  List.iter (fun (t,t') ->
      G.add_edge g t t';
      G.add_edge g t' t)
    syn
      
let add_subtags (subtags : (Tag.t * Tag.t) list) =
  List.iter (fun (t,t') ->
      G.add_edge g t t')
    subtags

let compute_trans_closure () =
  let module O = Oper.I(G) in
  ignore(O.add_transitive_closure g)

  
let get_subtags (tags : Tag.t list) : Tag.t list =
  List.concat_map (fun v ->
      (G.fold_pred (fun v xs -> v :: xs) g v []))
    tags
  |> List.sort_uniq (String.compare)
