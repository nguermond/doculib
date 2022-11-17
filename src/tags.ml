(******************************************************************************)
(* DocuLib                                                                    *)
(* Copyright (C) 2022 Nathan Guermond                                         *)
(*                                                                            *)
(* This program is free software: you can redistribute it and/or modify it    *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation, either version 3 of the License, or (at your option)  *)
(* any later version.                                                         *)
(*                                                                            *)
(* This program is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *)
(* for more details.                                                          *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License along    *)
(* with this program. If not, see <https://www.gnu.org/licenses/>.            *)
(*                                                                            *)
(******************************************************************************)
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

    
let g : t = init ()

    
let add_tags (tags : Tag.t list) =
  List.iter (fun t ->
      let v = G.V.create t in
      G.add_vertex g.graph v)
    tags

let add_synonyms (syn : pair list) =
  g.synonyms := List.append syn !(g.synonyms);
  List.iter (fun (t,t') ->
      G.add_edge g.graph t t';
      G.add_edge g.graph t' t)
    syn
  
let add_subtags (sub : pair list) =
  g.subtags := List.append sub !(g.subtags);
  List.iter (fun (t,t') ->
      G.add_edge g.graph t t')
    sub
  
let compute_trans_closure () =
  let module O = Oper.I(G) in
  ignore(O.add_transitive_closure g.graph)
  
let get_subtags (tags : Tag.t list) : Tag.t list =
  List.concat_map (fun v ->
      (if (G.mem_vertex g.graph v) then
         (G.fold_pred (fun v xs -> v :: xs) g.graph v [])
      else [v]))
    tags
  |> List.sort_uniq (Tag.compare)

let get_suptags (tags : Tag.t list) : Tag.t list =
  List.concat_map (fun v ->
      (if (G.mem_vertex g.graph v) then
         (G.fold_succ (fun v xs -> v :: xs) g.graph v [v])
       else [v]))
    tags
  |> List.sort_uniq (Tag.compare)
  
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
  add_synonyms synonyms;
  add_subtags subtags;
  compute_trans_closure ();
  g
  
let get_synonym_lst () =
  !(g.synonyms)

let get_subtag_lst () =
  !(g.subtags)



