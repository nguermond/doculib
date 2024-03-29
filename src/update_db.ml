
(* In case of change in data format in the database,
 * we need to mutate the entries to the new format.
 *)

exception CannotMigrate of string


(* Version 2.0 -> Version 2.1
 * Changes:
 * - add the md5 hash value of each document
 * Version 2.1 -> Version 3.0
 * Changes:
 * - library metadata is no longer stored in $HOME/.doculib/data,
 *   but is stored local to each library in library/.metadata
 * - library configuration is now stored in $XDG_CONFIG_HOME/doculib
 *   if it exists, otherwise in $HOME/.config/doculib
 * - path, doc_type, and hash are no longer stored
 * - change in libconfig format
 * Version 3.0 -> Version 3.1
 * Changes:
 * - add notes field
 *)

open Metadb


let homevar () : Path.root =
  match (Sys.getenv_opt "HOME") with
  | Some home -> Log.push (Format.sprintf "found HOME=%s\n" home);
                 Path.mk_root home
  | None -> raise(CannotMigrate("Cannot migrate: HOME var not set!"))


let usr_config () =
  try
    let usr_config = (XDGBaseDir.Config.user_file "doculib") in
    Log.push (Format.sprintf "found XDG_CONFIG_HOME=%s\n" usr_config);
    (Path.mk_root usr_config)
  with _ ->
    raise(CannotMigrate("Cannot find XDG Config directory!"))

            
(* Create a 3.1 doc from a 2.X doc *) 
let read_and_mutate_doc_2 file : Doc.t =
  let json = Json.from_file file in
  let open Json in
  { star = (to_bool (raise_opt "star" (get "star" json)));
    title = (to_string (raise_opt "title" (get "title" json)));
    authors = (List.map to_string
                 (to_list (raise_opt "authors" (get "authors" json))));
    doi = (to_string (raise_opt "doi" (get "doi" json)));
    isbn = (to_string (raise_opt "isbn" (get "isbn" json)));
    year = (to_string (raise_opt "year" (get "year" json)));
    tags = (List.map to_string
              (to_list (raise_opt "tags" (get "tags" json))));
    notes = "";
  }

(* Create a 3.1 doc from a 3.0 doc *)
let read_and_mutate_doc_3 file : Doc.t * Hash.t =
  let json = Json.from_file file in
  let open Json in
  let hash = Hash.of_string (to_string (raise_opt "hash" (get "hash" json))) in
  let json = (raise_opt "data" (get "data" json)) in
  let data : Doc.t = 
    { star = (to_bool (raise_opt "star" (get "star" json)));
      title = (to_string (raise_opt "title" (get "title" json)));
      authors = (List.map to_string
                   (to_list (raise_opt "authors" (get "authors" json))));
      doi = (to_string (raise_opt "doi" (get "doi" json)));
      isbn = (to_string (raise_opt "isbn" (get "isbn" json)));
      year = (to_string (raise_opt "year" (get "year" json)));
      tags = (List.map to_string
                (to_list (raise_opt "tags" (get "tags" json))));
      notes = "";
    } in
  (data,hash)

(* type of a 2.X library *)
type library_2 = {
    name : string;
    root : string;
    doc_type : string;
  }

(* type of a 3.X library *)
type library_3 = {
    name' : string;
    root' : Path.root;
    libdata' : Library.t;
  }

(* libconfig location for 2.X *)
let libconfig_2 () : Path.root =
  Path.merge_lst (homevar())
    [Path.mk_name ".doculib";
     Path.mk_name "libraries.json"]

(* data location for 2.1 *)
let data_dir_2_1 () : Path.root =
  Path.merge_lst (homevar())
    [Path.mk_name ".doculib";
     Path.mk_name "data";
     Path.mk_name "2.1"]

(* libconfig location for 3.X *)
let libconfig_3 () : Path.root =
  Path.merge_lst (usr_config())
    [(Path.mk_name "libraries.json")]

(* read a 2.X libconfig *)
let read_libconfig_2 () : string * ((string * library_2) list) =
  Log.push "reading libconfig 2.X";
  let json = Json.from_file (libconfig_2()) in
  let version = Json.to_string(Json.get_err "version" json) in
  let libs =
    (Json.to_list (Json.raise_opt "`libraries`"
                     (Json.get "libraries" json)))
    |> List.map (fun json ->
           let name = Json.to_string (Json.raise_opt "`name`" (Json.get "name" json)) in
           let path = Json.to_string (Json.raise_opt "`path`" (Json.get "path" json)) in
           let doc_type = Json.to_string (Json.raise_opt "`doc_type`"
                                            (Json.get "doc_type" json)) in
           (name,{name = name;
                  root = path;
                  doc_type = doc_type}))
  in (version,libs)

(* read a 3.X libconfig *)
let read_libconfig_3 () : ((string * library_3) list) =
  Log.push "reading libconfig 3.X";
  let json = Json.from_file (libconfig_3()) in
  let libs =
    Json.to_list json
    |> List.map (fun json ->
           let name = Json.to_string (Json.raise_opt "`name`" (Json.get "name" json)) in
           let root = Json.to_string (Json.raise_opt "`root`" (Json.get "root" json)) in
           let libdata = (Json.raise_opt "`libdata`" (Json.get "libdata" json)) in
           (name,{name' = name;
                  root' = Path.mk_root root;
                  libdata' = Library.from_json libdata}))
  in libs
   
(* write a 3.X libconfig *)
let write_libconfig (libs : library_3 list) : unit =
  let to_json lib : Json.t =
    (`Assoc [("name",`String lib.name');
             ("root", `String (Path.string_of_root lib.root'));
             ("libdata", (Library.to_json lib.libdata'))
    ])
  in
  let json = (`List (List.map to_json libs)) in
  (System.make_dirp (libconfig_3()));
  (Json.to_file (libconfig_3()) json)


(* mutate a 2.X library to a 3.1 library *)
let mutate_library_2 ~library root : unit =
  Log.push (Format.sprintf "Migrating library: %s @ %s"
              library (Path.string_of_root root));
  let library_md = (Path.merge_lst (data_dir_2_1())
                      [Path.mk_name library]) in
  let files = List.of_seq (System.get_files ~hidden:true library_md) in
  let new_library_md = Path.merge_lst root
                         [Path.mk_name ".metadata"] in
  List.iter (fun file ->
      let name = Path.strip_root library_md file in
      let new_file = Path.merge new_library_md name in
      let physical_file = (Path.remove_file_ext "json" (Path.merge root name)) in
      let hash = (Hash.to_string @@ Hash.hash_file physical_file) in
      let json =
        (`Assoc [("data", Doc.to_json(read_and_mutate_doc_2 file));
                 ("hash", `String hash)])
      in
      Log.push (Format.sprintf "Moving %s\n -> %s"
                  (Path.string_of_root file)
                  (Path.string_of_root new_file));
      (System.make_dirp new_file);
      (Json.to_file new_file json)
    )
    files

(* mutate a 3.X library to a 3.1 library *)
let mutate_library_3 ~library root : unit =
  Log.push (Format.sprintf "Migrating library: %s @ %s"
              library (Path.string_of_root root));
  let library_md = Path.merge_lst root
                     [Path.mk_name ".metadata"] in
  let files = (List.of_seq (System.get_files ~hidden:true library_md)) in
  Log.push (Format.sprintf "Updating %d files in %s\n" (List.length files)
              (Path.string_of_root library_md));
  List.iter (fun file ->
      let doc,hash = (read_and_mutate_doc_3 file) in
      let json = (`Assoc [("data", Doc.to_json(doc));
                          ("hash", `String (Hash.to_string hash))])
      in
      Log.push (Format.sprintf "Updating %s"
                  (Path.string_of_root file));
      (Json.to_file file json)
    )
    files
    

(* Create libconfig for 3.1 library from a 2.X library *)
let mutate_libconfig_2 (libs : (string * library_2) list) : unit =
  Log.push "mutating libconfig 2.X to 3.1";
  let new_libs =
    List.map (fun (library, lib) ->
        {name' = lib.name;
         root' = Path.mk_root lib.root;
         libdata' = {version = "3.1";
                     doc_type = (Library.doc_type_of_string lib.doc_type)
      }})
      libs
  in (write_libconfig new_libs)

(* Create libconfig for 3.1 library from a 3.X library *)
let mutate_libconfig_3 (libs : (string * library_3) list) : unit =
  Log.push "mutating libconfig 3.X to 3.1";
  let new_libs =
    List.map (fun (library, lib) ->
        {name' = lib.name';
         root' = lib.root';
         libdata' = {version = "3.1";
                     doc_type = lib.libdata'.doc_type}
      })
      libs
  in (write_libconfig new_libs)

(* Mutate a database of 2.X libraries to a database of 3.1 libraries *)
let mutate_db_2 (libs : (string * library_2) list) : unit =
  Log.push "mutating 2.X libraries to 3.1 libraries";
  List.iter (fun (library, lib) ->
      (mutate_library_2 ~library (Path.mk_root lib.root)))
    libs

(* Mutate a database of 3.X libraries to a database of 3.1 libraries *)
let mutate_db_3 (libs : (string * library_3) list) : unit =
  Log.push "Checking library 3.1 compatibility";
  let change = ref false in
  List.iter (fun (library, lib) ->
      let version = float_of_string lib.libdata'.version in
      if version < 3.1 then
        (mutate_library_3 ~library lib.root'; change := true))
    libs;
  if !change then
    (mutate_libconfig_3 libs)

(* Assume 3.X environment does not already exist,
 * and 2.X environment exists *)
let update_2_to_3_1 () : unit =
  Log.push(Format.sprintf "Upgrading library from version 2.X to 3.1");
  let version,libs = read_libconfig_2() in
  let version = float_of_string version in
  if version < 2.1 then
    raise (CannotMigrate("Cannot migrate library version < 2.1 from this version of Doculib.\nUse the previous version of doculib to upgrade library to 2.1, then try again."));
  (* otherwise version must be 2.1 *)
  (mutate_libconfig_2 libs);
  (mutate_db_2 libs)

let update_3_to_3_1 () : unit =
  let libs = read_libconfig_3() in
  (mutate_db_3 libs)
  
let init () : unit =
  if (System.file_exists (libconfig_3())) then
    (update_3_to_3_1 ())
  else if (System.file_exists (libconfig_2())) then
    (update_2_to_3_1 ())
  else
    (* libconfig will be created *)
    ()
