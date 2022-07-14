(******************************************************************************)
(* Metadb                                                                     *)
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

exception FileExists of Path.root
exception EntryExists of (string * Path.rel)
exception EntryDoesNotExist of (string * Path.rel)

exception DirNotEmpty of Path.root

exception LibraryExists

exception InternalError


module Path = Path
module Hash = Hash
module Json = Json
module System = System
              
        
module type Metadata =
sig
  type t
  val to_json : t -> Json.t
  val from_json : Json.t -> t
  val init : t
  val merge : t -> t -> t option
  val to_string : t -> string
end

module Entry (D : Metadata) =
  struct
    type t = {
        data : D.t;
        hash : Hash.t;
        mutable modified : bool;
      }

    let make (d : D.t) (h : Hash.t) : t =
      {data = d; hash = h; modified = true}

    let empty (e : t) : bool =
      e.data = D.init

    let set_modified (e : t) : unit = e.modified <- true
      
    let modified (e : t) : bool = e.modified

    let to_json (e : t) : Json.t =
      (`Assoc [("data", (D.to_json e.data));
               ("hash", `String (Hash.to_string e.hash))])
      
    let from_json (j : Json.t) : t =
      let jmd = (Json.get_err "data" j) in
      let h = (Json.to_string (Json.get_err "hash" j)) in
      let d = D.from_json jmd in
      {data = d;
       hash = (Hash.of_string h);
       modified = false;
      }

    let write_file (file : Path.root) (e : t) : unit =
      let j = to_json e in
      (if not (System.file_exists file) then System.make_dirp file);
      Json.to_file file j;
      e.modified <- false
      
    let read_file (path : Path.root) : t =
      let j = Json.from_file path in
      from_json j

    let get_data (e : t) : D.t = e.data
    let get_hash (e : t) : Hash.t = e.hash

    let to_string (path : Path.rel) (e : t) : string =
      Format.sprintf "%s => %s:@\n@[<2>%s@]\n"
        (Path.string_of_rel path) (Hash.to_string e.hash) (D.to_string e.data)
  end

  
              
type file_table = (Hash.t * (string * Path.rel)) list


                
module type LibData =
  sig
    type t
    val to_json : t -> Json.t
    val from_json : Json.t -> t
  end

module Library (D : Metadata) (LD : LibData) =
  struct
    module E = Entry(D)
    type t = {
        mutable name : string;
        root : Path.root;
        libdata : LD.t;
        entries : (Path.rel, E.t) Hashtbl.t;
      }

           
    let rename (lib : t) new_name : t =
      lib.name <- new_name; lib

    let store (lib : t) : Path.root =
      Path.merge lib.root (Path.mk_rel ".metadata")

    let load_entries (lib : t) : unit =
      let root = (store lib) in
      (Seq.iter 
         (fun path ->
           let entry = (E.read_file path) in
           let path = (Path.remove_file_ext "json" path) in
           let key = (Path.strip_root root path) in
           Hashtbl.add lib.entries key entry)
         (System.get_files ~hidden:true root))

    let entry_empty (lib : t) (key : Path.rel) : bool =
      E.empty (Hashtbl.find lib.entries key)
      
    let entry_exists (lib : t) (key : Path.rel) : bool =
      match (Hashtbl.find_opt lib.entries key) with
      | None -> false
      | Some _ -> true

    let file_exists (lib : t) (key : Path.rel) : bool =
      let path = (Path.merge lib.root key) in
      System.file_exists path

    (* Add entries for new files *)
    let read_files (lib : t) : (Path.rel * E.t) Seq.t =
      let root = lib.root in
      Seq.filter_map
        (fun path ->
          let key = (Path.strip_root root path) in
          if (entry_exists lib key) then None
          else
            let d = D.init in
            let h = Hash.hash_file path in
            let entry = (E.make d h) in
            Hashtbl.add lib.entries key entry;
            Some (key,entry))
        (System.get_files root)
        

    let init (lib : t) : unit =
      if not(System.file_exists (store lib)) then
        System.make_dirp_leaf (store lib);
      load_entries lib;
      ignore @@ (read_files lib ())

    let refresh (lib : t) : (Path.rel * D.t) Seq.t =
      Seq.map (fun (path,e) -> (path,E.get_data e))
        (read_files lib)

    let make (name : string) (root : Path.root) (libdata : LD.t) : t =
      {name = name;
       root = root;
       libdata = libdata;
       entries = Hashtbl.create 1
      }
      
    let to_json (lib : t) : Json.t =
      (`Assoc [("name",`String lib.name);
               ("root", `String (Path.string_of_root lib.root));
               ("libdata", (LD.to_json lib.libdata))
      ])
      
    let from_json (json : Json.t) : string * t =
      let jname = (Json.get_err "name" json) in
      let jroot = (Json.get_err "root" json) in
      let jdata = (Json.get_err "libdata" json) in
      let name = (Json.to_string jname) in
      let root = (Path.mk_root (Json.to_string jroot)) in
      let libdata = LD.from_json jdata in
      (name, make name root libdata)
      
    let add (lib : t) (key : Path.rel) (m : D.t) : unit =
      let path = (Path.merge lib.root key) in
      (if (file_exists lib key) then
         raise (FileExists path));
      let h = Hash.hash_file path in
      let entry = E.make m h in
      (Hashtbl.add lib.entries key entry)
      
    let get (lib : t) (key : Path.rel) : D.t option =
      match (Hashtbl.find_opt lib.entries key) with
      | Some e -> Some (E.get_data e)
      | None -> None

    let set (lib : t) (key : Path.rel) (m : D.t) : unit =
      match (Hashtbl.find_opt lib.entries key) with
      | Some e ->
         let entry = E.make m (E.get_hash e) in
         Hashtbl.replace lib.entries key entry
      | None -> raise (EntryDoesNotExist(lib.name,key))

    let set_entry (lib : t) (key : Path.rel) (e : E.t) : unit =
      E.set_modified e;
      Hashtbl.replace lib.entries key e

    let get_entry (lib : t) (key : Path.rel) : E.t option =
      Hashtbl.find_opt lib.entries key

    let remove_entry (lib : t) (key : Path.rel) : unit =
      Hashtbl.remove lib.entries key;
      let file = (Path.add_file_ext "json" (Path.merge (store lib) key)) in
      if System.file_exists file then
        System.remove file

    let remove_file (lib : t) (key : Path.rel) : unit =
      System.remove (Path.merge lib.root key)
      
    (* assumes new entry does not exists *)
    let remap (lib : t) (key : Path.rel) (key' : Path.rel) : unit =
      let e = Hashtbl.find lib.entries key in
      if (entry_exists lib key') then
        raise(EntryExists(lib.name,key'))
      else
        (remove_entry lib key;
         Hashtbl.add lib.entries key' e)
      
    let index_files (lib : t) (tbl : file_table) : file_table =
      Hashtbl.fold (fun key e tbl ->
          let library = lib.name in
          let hash = E.get_hash e in
          if (file_exists lib key) then
            ((hash,(library, key))::tbl)
          else tbl)
        lib.entries tbl

    (* Return the list of entries 
     * that do not have associated files *)
    let get_unmatched_entries (lib : t) : (Path.rel * E.t) Seq.t =
      Seq.filter (fun (key,e) -> not (file_exists lib key))
        (Hashtbl.to_seq lib.entries)

    (* write modified entries to disk *)
    let flush_modified_entries (lib : t) : unit =
      Hashtbl.iter 
        (fun key entry ->
          if (E.modified entry) then
            let path = (Path.merge (store lib) key) in
            let file = (Path.add_file_ext "json" path) in
            E.write_file file entry
          else ())
        lib.entries

    let get_root (lib : t) : Path.root =
      lib.root

    let get_libdata (lib : t) : LD.t =
      lib.libdata
      
    let to_string library (lib : t) : string =
      let str = Hashtbl.fold (fun key e str ->
                    (E.to_string key e) ^ str)
                  lib.entries "" in
      Format.sprintf "%s = {@\n@[<2>%s@]\n" library str

    let get_entries (lib : t) : (Path.rel * D.t) Seq.t =
      Seq.map (fun (path,e) -> (path,E.get_data e))
        (Hashtbl.to_seq lib.entries)
  end

module Make (D : Metadata) (LD : LibData) =
  struct
    module L = Library(D)(LD)
    module E = Entry(D)
             
    let libraries : ((string * L.t) list) ref = ref []
                                              
    (* We keep a global index of files by their hash to deal
     * with moved/renamed files and duplicates.
     * file_index : file_hash -> (library, file_path) *)
    let file_index : file_table ref = ref []

    let refresh_library ~library : (Path.rel * D.t) Seq.t =
      L.refresh (List.assoc library !libraries)
      
    let init_library ~library : unit =
      L.init (List.assoc library !libraries)

    let init_libraries () : unit =
      List.iter (fun (name,lib) -> L.init lib) !libraries

    let get_entry ~library (key : Path.rel) : D.t option =
      let lib = (List.assoc library !libraries) in
      L.get lib key

    let set_entry ~library (key : Path.rel) (m : D.t) : unit =
      let lib = (List.assoc library !libraries) in
      L.set lib key m

    let remove_entry ~library (key : Path.rel) : unit =
      let lib = (List.assoc library !libraries) in
      L.remove_entry lib key

    let remove_file ~library (key : Path.rel) : unit =
      let lib = (List.assoc library !libraries) in
      L.remove_file lib key      

    let new_library ~library (root : Path.root) (libdata : LD.t) : unit =
      if (List.mem_assoc library !libraries) then
        raise (LibraryExists);
      let lib = L.make library root libdata in
      (* Note: Entries may already be present *)
      L.init lib; 
      ignore @@ (L.read_files lib ());
      libraries := (library, lib) :: !libraries

    let remove_library ~delete_metadata ~library : unit =
      let lib = List.assoc library !libraries in
      libraries := List.remove_assoc library !libraries;
      if delete_metadata && (System.file_exists (L.store lib)) then
        System.rmdir (L.store lib)

    let rename_library ~library new_name : unit =
      let lib = List.assoc library !libraries in
      let lib = L.rename lib new_name in
      remove_library ~delete_metadata:false ~library;
      libraries := (new_name,lib) :: !libraries

    let move_library ~library (root : Path.root) : unit =
      let lib = List.assoc library !libraries in
      if (System.file_exists root) then
        if not (System.empty_dir root) then
          raise(DirNotEmpty root)
        else
          System.move (L.get_root lib) (Path.drop_leaf root)
      else
        let _ = System.make_dirp root in
        System.move (L.get_root lib) root

    let index_files () : unit =
      file_index :=
        List.fold_left (fun tbl (library,lib) ->
            L.index_files lib tbl)
          [] !libraries 

    (* Move entry and file from one library to another *)
    let migrate_entry ~from_lib ~to_lib (key : Path.rel) : unit =
      let from_lib_ = (List.assoc from_lib !libraries) in
      let to_lib_ = (List.assoc to_lib !libraries) in
      if (L.file_exists to_lib_ key) then
        (* Output a warning message *)
        raise(FileExists (Path.merge (L.get_root to_lib_) key))
      else if (L.entry_exists to_lib_ key) then
        raise(EntryExists (to_lib, key))
      else
        (match L.get_entry from_lib_ key with
         | Some entry ->
            (L.set_entry to_lib_ key entry);
            (L.remove_entry from_lib_ key);
            (* Note: File may be missing, but this is okay,
             * we move the entry anyways *)
            if (L.file_exists from_lib_ key) then
              let from_path = Path.merge (L.get_root from_lib_) key in
              let to_path = Path.merge (L.get_root to_lib_) key in
              let _ = (System.make_dirp to_path) in
              (System.move from_path to_path)
         | None -> raise(EntryDoesNotExist(from_lib,key)))


    type resolution = Remap of (Path.rel * (string * Path.rel))
                    | Missing of Path.rel

    let remap_entry ~from_lib ~to_lib key key' : (Path.rel * (string * Path.rel)) =
      let lib = (List.assoc from_lib !libraries) in
      let lib' = (List.assoc to_lib !libraries) in
      match L.get_entry lib key, L.get_entry lib' key' with
      | Some entry, None ->
         L.set_entry lib' key' entry;
         L.remove_entry lib key;
         (key,(to_lib,key'))
      | Some entry, Some entry' ->
         (match D.merge (E.get_data entry) (E.get_data entry') with
          | Some d -> prerr_endline "Remap: Merging entries";
             let hash = E.get_hash entry' in
             L.set_entry lib' key' (E.make d hash);
             L.remove_entry lib key;
             (key,(to_lib,key'))
          | None -> raise(EntryExists(to_lib,key')))
      | None, _ -> raise(InternalError)
        
    (* This function assumes 
     * 1. libraries are freshly initialized or have been refreshed
     * 2. files have been indexed *)
    let resolve_missing_files ~library : resolution Seq.t =
      let lib = List.assoc library !libraries in
      let entries = (L.get_unmatched_entries lib) in
      let resolutions =
        Seq.filter_map (fun (key,entry) ->
            let hash = (L.E.get_hash entry) in
            match (List.assoc_opt hash !file_index) with
            | Some (library',key') ->
               (try Some(Remap (remap_entry ~from_lib:library
                               ~to_lib:library' key key'))
                with _ ->
                  (Some (Missing key)))
            | None ->
               Some (Missing key))
          entries
      in
      resolutions

    (* Return type is a partition of the duplicate files, of the form:
        [[(lib_11, file_11), (lib_12, file_12),...]
         [(lib_21, file_21), (lib_22, file_22),...]
         ...
         [(lib_n1, file_n1), (lib_n2, file_n2),...]]
       such that entries in each row are duplicates
     *)
    (* TODO: Attempt to merge duplicates if one is more precise than another 
     * use D.merge *)
    let find_duplicates () : ((string * Path.rel) list) list =
      let rec find_dups dup_hashes dups bdgs =
        match bdgs, dup_hashes, dups with
        | [], _, dups -> dups
        | [(hash,v)], [], [] -> []
        | [(hash,v)], h::_, dup::dups ->
           if ((hash = h))
           then
             ((v::dup)::dups)
           else (dup::dups)
        | (hash,v)::bdgs, h::_, dup::dups -> 
           if (hash = h) then
             (find_dups dup_hashes ((v::dup)::dups) bdgs)
           else if (hash = fst (List.hd bdgs)) then
             (find_dups (hash::dup_hashes) ([v]::(dup::dups)) bdgs)
           else (find_dups dup_hashes (dup::dups) bdgs)
        | (hash,v)::bdgs, [], [] ->
           if (hash = fst (List.hd bdgs)) then
             (find_dups ([hash]) ([[v]]) bdgs)
           else
             (find_dups [] [] bdgs)
        | _ -> raise (InternalError)
      in
      let bdgs = (List.sort (fun (h,u) (h',u') ->
                      Hash.compare h h')
                    !file_index) in
      (find_dups [] [] bdgs)
      
    let to_json () : Json.t =
      (`List (List.map (fun (library,lib) ->
                  L.to_json lib)
                !libraries))
        
    let from_json (json : Json.t) : (string * L.t) list =
      let libs = Json.to_list json in
      (List.map L.from_json libs)


    let load_config (libconfig : Path.root) : unit =
      if System.file_exists libconfig then
        let libs = from_json @@ Json.from_file libconfig in
        libraries := libs
      else
        (System.make_dirp libconfig;
         libraries := [])
        

    let write_config ?(ord = []) (libconfig : Path.root) : unit =
      libraries := 
        (if ord = [] then !libraries else
           (List.init (List.length !libraries) (fun i ->
                let library = (List.nth ord i) in
                (library, List.assoc library !libraries))));
      let jlibs = to_json () in
      Json.to_file libconfig jlibs

    let get_libdata () : (string * LD.t) list =
      List.map (fun (library,lib) ->
          (library, L.get_libdata lib))
      !libraries

    let get_library_root ~library : Path.root =
      L.get_root (List.assoc library !libraries)

    let get_entries ~library : (Path.rel * D.t) Seq.t =
      let lib = List.assoc library !libraries in
      L.get_entries lib

    let flush_library_metadata ~library : unit =
      L.flush_modified_entries (List.assoc library !libraries)
      
    let flush_metadata () : unit =
      List.iter (fun (library,lib) ->
          L.flush_modified_entries lib)
        !libraries

    let library_to_string ~library : string =
      L.to_string library (List.assoc library !libraries)
  end

