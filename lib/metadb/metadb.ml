
exception FileExists of Path.root
exception EntryExists of (string * Path.rel)
exception EntryDoesNotExist of (string * Path.rel)

exception DirNotEmpty of Path.root 


module type Metadata =
sig
  type t
  val to_json : t -> Json.t
  val from_json : Json.t -> t
  val init : t
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

    let to_file (path : Path.root) (e : t) : unit =
      let j = to_json e in
      (if not (Sys.file_exists path) then Sys.make_dirp path);
      Json.to_file path j;
      e.modified <- false
      
    let from_file (path : Path.root) : t =
      let j = Json.from_file path in
      from_json j

    let get_data (e : t) : D.t = e.data
    let get_hash (e : t) : Hash.t = e.hash

    let to_string (path : Path.rel) (e : t) : string =
      Format.sprintf "%s => %s:@\n@[<2>%s@]\n"
        (Path.rel_to_string path) (Hash.to_string e.hash) (D.to_string e.data)
  end

  
module FileTbl = Hashtbl.Make(struct
                     type t = Hash.t
                     let equal = Hash.equal
                     let hash = Hash.to_int
                   end)
type file_table = (string * Path.rel) FileTbl.t


                
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
        name : string;
        root : Path.root;
        libdata : LD.t;
        entries : (Path.rel, E.t) Hashtbl.t;
      }

      
    let store (lib : t) : Path.root =
      Path.merge lib.root (Path.mk_rel ".metadata")

    let load_entries (lib : t) : unit =
      let root = (store lib) in
      ignore @@
        Sys.get_files_map root
        (fun path ->
          let json = (Json.from_file path) in
          let entry = (E.from_json json) in
          let path = (Path.remove_file_ext "json" path) in
          let key = (Path.strip_root root path) in
          Hashtbl.add lib.entries key entry)

    let entry_empty (lib : t) (key : Path.rel) : bool =
      E.empty (Hashtbl.find lib.entries key)
      
    let entry_exists (lib : t) (key : Path.rel) : bool =
      match (Hashtbl.find_opt lib.entries key) with
      | None -> false
      | Some _ -> true

    let file_exists (lib : t) (key : Path.rel) : bool =
      let path = (Path.merge lib.root key) in
      Sys.file_exists path

    (* Add entries for new files *)
    let read_files (lib : t) : unit =
      let root = lib.root in
      ignore @@
        Sys.get_files_map root
          (fun path ->
            let key = (Path.strip_root root path) in
            if (entry_exists lib key) then ()
            else
              let d = D.init in
              let h = Hash.hash_file path in
              let entry = (E.make d h) in
              Hashtbl.add lib.entries key entry)

    let init (lib : t) : unit =
      load_entries lib;
      read_files lib

    let refresh (lib : t) : unit =
      read_files lib

    let make (name : string) (root : Path.root) (libdata : LD.t) : t =
      {name = name;
       root = root;
       libdata = libdata;
       entries = Hashtbl.create 1
      }
      
    let to_json (lib : t) : Json.t =
      (`Assoc [("name",`String lib.name);
               ("root", `String (Path.to_string lib.root));
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

    let remove (lib : t) (key : Path.rel) : unit =
      Hashtbl.remove lib.entries key;
      Sys.remove (Path.merge lib.root key)

    (* assumes new entry does not exists *)
    let remap (lib : t) (key : Path.rel) (key' : Path.rel) : unit =
      let e = Hashtbl.find lib.entries key in
      if (entry_exists lib key') then
        raise(EntryExists(lib.name,key'))
      else
        (remove lib key;
         Hashtbl.add lib.entries key' e)
      
    let index_files (lib : t) (tbl : file_table) : unit =
      Hashtbl.iter (fun key e ->
          let library = lib.name in
          let hash = E.get_hash e in
          FileTbl.add tbl hash (library, key))
        lib.entries

    (* Return the list of entries 
     * that do not have associated files *)
    let get_unmatched_entries (lib : t) : (Path.rel * E.t) list =
      Hashtbl.fold (fun key e entries ->
          (if (file_exists lib key) then entries
           else ((key,e) :: entries)))
        lib.entries []

    (* write modified entries to disk *)
    let flush_modified_entries (lib : t) : unit =
      Hashtbl.iter 
        (fun key entry ->
          if (E.modified entry) then
            let path = (Path.merge (store lib) key) in
            E.to_file path entry
          else ())
        lib.entries

    let get_root (lib : t) : Path.root =
      lib.root

    let to_string library (lib : t) : string =
      let str = Hashtbl.fold (fun key e str ->
                    (E.to_string key e) ^ str)
                  lib.entries "" in
      Format.sprintf "%s = {@\n@[<2>%s@]\n" library str
  end
                 
module Make (D : Metadata) (LD : LibData) =
  struct
    module L = Library(D)(LD)
    let libraries : ((string * L.t) list) ref = ref []
                                              
    (* We keep a global index of files by their hash to deal
     * with moved/renamed files and duplicates.
     * file_index : file_hash -> (library, file_path) *)
    let file_index : file_table = FileTbl.create 1


    let refresh_library ~library : unit =
      L.refresh (List.assoc library !libraries)
      
    let init_library ~library : unit =
      L.init (List.assoc library !libraries)

    let refresh_libraries () : unit =
      List.iter (fun (name,lib) -> L.refresh lib) !libraries
      
    let init_libraries () : unit =
      List.iter (fun (name,lib) -> L.init lib) !libraries

    let add_entry ~library (key : Path.rel) (m : D.t) : unit =
      let lib = (List.assoc library !libraries) in
      L.add lib key m

    let get_entry ~library (key : Path.rel) : D.t option =
      let lib = (List.assoc library !libraries) in
      L.get lib key

    let set_entry ~library (key : Path.rel) (m : D.t) : unit =
      let lib = (List.assoc library !libraries) in
      L.set lib key m

    let remove_entry ~library (key : Path.rel) : unit =
      let lib = (List.assoc library !libraries) in
      L.remove lib key

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
        (match L.get from_lib_ key with
         | Some entry ->
            (L.set to_lib_ key entry);
            (L.remove from_lib_ key);
            (* Note: File may be missing, but this is okay,
             * we move the entry anyways *)
            if (L.file_exists from_lib_ key) then
              let from_path = Path.merge (L.get_root from_lib_) key in
              let to_path = Path.merge (L.get_root to_lib_) key in
              let _ = (Sys.make_dirp to_path) in
              (Sys.move from_path to_path)
         | None -> raise(EntryDoesNotExist(from_lib,key)))

    let remap_entry ~from_lib ~to_lib key key' : unit =
      let from_lib_ = (List.assoc from_lib !libraries) in
      let to_lib_ = (List.assoc to_lib !libraries) in
      if (L.file_exists to_lib_ key') then
        raise(FileExists (Path.merge (L.get_root to_lib_) key'))
      else if (L.entry_exists to_lib_ key'
               && not(L.entry_empty to_lib_ key')) then
        raise(EntryExists (to_lib, key))
      else
        failwith "NYI"
        (* (match L.get from_lib_ key with
         *  | Some entry ->
         *     (L.set to_lib_ key entry);
         *     (L.remove from_lib_ key);
         *     (\* Note: File may be missing, but this is okay,
         *      * we move the entry anyways *\)
         *     if (L.file_exists from_lib_ key) then
         *       let from_path = Path.merge (L.get_root from_lib_) key in
         *       let to_path = Path.merge (L.get_root to_lib_) key' in
         *       let _ = (Sys.make_dirp to_path) in
         *       (Sys.move from_path to_path)
         *  | None -> raise(EntryDoesNotExist key)) *)
      
      
    let new_library ~library (root : Path.root) (libdata : LD.t) : unit =
      let lib = L.make library root libdata in
      L.load_entries lib; (* Entries may already be present *)
      L.read_files lib;
      libraries := (library, lib) :: !libraries

    let remove_library ~library : unit =
      libraries := List.remove_assoc library !libraries

    let rename_library ~library new_name : unit =
      let lib = List.assoc library !libraries in
      remove_library ~library;
      libraries := (library,lib) :: !libraries

    let move_library ~library (root : Path.root) : unit =
      let lib = List.assoc library !libraries in
      if (Sys.file_exists root) then
        if not (Sys.empty_dir root) then
          raise(DirNotEmpty root)
        else
          Sys.move (L.get_root lib) (Path.drop_leaf root)
      else
        let _ = Sys.make_dirp root in
        Sys.move (L.get_root lib) root

    let index_files () : unit =
      FileTbl.reset file_index;
      List.iter (fun (library,lib) ->
          L.index_files lib file_index)
        !libraries

    (* This function assumes 
     * 1. libraries are freshly initialized or have been refreshed
     * 2. files have been indexed *)
    let resolve_missing_files ~library : unit =
      let lib = List.assoc library !libraries in
      let entries = (L.get_unmatched_entries lib) in
      List.iter (fun (key,entry) ->
          let hash = L.E.get_hash entry in
          match (FileTbl.find_opt file_index hash) with
          | Some (library',key') when library=library' ->
             L.remap lib key key'
          | Some (library',key') ->
             let lib' = List.assoc library' !libraries in
             if (not (L.entry_exists lib' key') ||
                   (L.entry_empty lib' key')) then
               (remap_entry ~from_lib:library
                  ~to_lib:library' key key')
             else
               raise(EntryExists(library',key'))
          | None -> failwith "NYI")
        entries

    let to_json () : Json.t =
      (`List (List.map (fun (name,lib) ->
                  (L.to_json lib))
                !libraries))
      
    let from_json (json : Json.t) : (string * L.t) list =
      let libs = Json.to_list json in
      (List.map L.from_json libs)


    let load_config (libconfig : Path.root) : unit =
      let libs = from_json @@ Json.from_file libconfig in
      libraries := libs

    let write_config (libconfig : Path.root) : unit =
      let jlibs = to_json () in
      Json.to_file libconfig jlibs
  end

