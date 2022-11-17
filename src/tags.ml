open Graph

module Tag =
  struct
      type t = string
      let compare = String.compare
      let equal = String.equal
      let hash x = String.length x
  end
         
module G = Imperative.Digraph.ConcreteBidirectional(Tag)

type pair = Tag.t * Tag.t
type t =
  { graph : G.t;
    synonyms : (pair list) ref;
    subtags : (pair list) ref
  }


let init () : t =
  { graph = G.create ();
    synonyms = ref [];
    subtags = ref [];
  }
    
let add_tags (g : t) (tags : Tag.t list) =
  List.iter (fun t ->
      let v = G.V.create t in
      G.add_vertex g.graph v)
    tags

let add_synonyms (g : t) (syn : (Tag.t * Tag.t) list) =
  g.synonyms := List.append syn !(g.synonyms);
  List.iter (fun (t,t') ->
      G.add_edge g.graph t t';
      G.add_edge g.graph t' t)
    syn
  
let add_subtags (g : t) (sub : (Tag.t * Tag.t) list) =
  g.subtags := List.append sub !(g.subtags);
  List.iter (fun (t,t') ->
      G.add_edge g.graph t t')
    sub
  
let compute_trans_closure (g : t) =
  let module O = Oper.I(G) in
  ignore(O.add_transitive_closure g.graph)
  
let get_subtags (g : t) (tags : Tag.t list) : Tag.t list =
  List.concat_map (fun v ->
      (G.fold_pred (fun v xs -> v :: xs) g.graph v []))
    tags
  |> List.sort_uniq (String.compare)

let load_from_file (path : Metadb.Path.root) : t =
  let module Json = Metadb.Json in
  let j = Json.from_file path in
  let version = Json.get_err "version" j in
  let synonyms = Json.to_list (Json.get_err "synonyms" j) in
  let subtags = Json.to_list (Json.get_err "subtags" j) in
  let synonyms = List.map (fun j ->
                     let pair = Json.to_list j in
                     let (x,y) = (List.nth pair 0, List.nth pair 1) in
                     (Json.to_string x, Json.to_string y))
                   synonyms in
  let subtags = List.map (fun j ->
                    let pair = Json.to_list j in
                    let (x,y) = (List.nth pair 0, List.nth pair 1) in
                    (Json.to_string x, Json.to_string y))
                  subtags in
  let g = init() in
  add_synonyms g synonyms;
  add_subtags g subtags;
  g
  
let get_synonyms (g : t) =
  !(g.synonyms)

let get_subtags (g : t) =
  !(g.subtags)
