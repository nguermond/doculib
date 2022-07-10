
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
 *)

open Metadb
   

let read_and_mutate_doc file : Doc.t =
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
  }

  
type old_library = {
    name : string;
    root : string;
    doc_type : string;
  }

type new_library = {
    name' : string;
    root' : Path.root;
    libdata' : Library.t;
  }


let libconfig_2 : Path.root =
  (match (Sys.getenv_opt "HOME") with
   | Some home -> Path.merge (Path.mk_root home)
                    (Path.mk_rel ".doculib/libraries.json")
   | None -> raise (CannotMigrate "$HOME variable is not set!"))

let data_dir_2_1 : Path.root =
  (match (Sys.getenv_opt "HOME") with
   | Some home -> Path.merge (Path.mk_root home)
                    (Path.mk_rel ".doculib/data/2.1")
   | None -> raise (CannotMigrate "$HOME variable is not set!"))

                          
let libconfig_3 : Path.root =
  match (Sys.getenv_opt "XDG_CONFIG_HOME") with
  | Some usr_config ->
     Path.merge (Path.mk_root usr_config)
       (Path.mk_rel "doculib/libraries.json")
  | None -> 
     (match (Sys.getenv_opt "HOME") with
      | Some home ->
         Path.merge (Path.mk_root home)
           (Path.mk_rel ".config/doculib/libraries.json")
      | None ->
         raise (CannotMigrate "$HOME environment variable is not set!"))
    
let read_libconfig () : string * ((string * old_library) list) =
  let json = Json.from_file libconfig_2 in
  let version = Json.to_string(Json.get_err "version" json) in
  let libs =
    (Json.to_list (Json.raise_opt "`libraries`"
                     (Json.get "libraries" json)))
    |> List.map (fun json ->
           let name = Json.to_string (Json.raise_opt "`name`" (Json.get "name" json)) in
           let path = Json.to_string (Json.raise_opt "`path`" (Json.get "path" json)) in
           let doc_type = Json.to_string (Json.raise_opt "`doc_type`"
                                            (Json.get "doc_type" json)) in
           (name,{name = name; root = path; doc_type = doc_type}))
  in (version,libs)


let write_libconfig (libs : new_library list) : unit =
  let to_json lib : Json.t =
    (`Assoc [("name",`String lib.name');
             ("root", `String (Path.string_of_root lib.root'));
             ("libdata", (Library.to_json lib.libdata'))
    ])
  in
  let json = (`List (List.map to_json libs)) in
  (System.make_dirp libconfig_3);
  (Json.to_file libconfig_3 json)


      
let mutate_library ~library root : unit =
  Log.push (Format.sprintf "Migrating library: %s @ %s"
              library (Path.string_of_root root));
  let library_md = (Path.merge_lst data_dir_2_1 [Path.mk_name library]) in
  let files = (System.get_files library_md) in
  let new_library_md = Path.merge_lst root [Path.mk_name ".metadata"] in
  Seq.iter (fun file ->
      let name = Path.strip_root library_md file in
      let new_file = Path.merge new_library_md name in
      let physical_file = (Path.remove_file_ext "json" (Path.merge root name)) in
      let hash = (Hash.to_string @@ Hash.hash_file physical_file) in
      let json =
        (`Assoc [("data", Doc.to_json(read_and_mutate_doc file));
                 ("hash", `String hash)])
      in
      prerr_endline (Format.sprintf "Moving %s\n -> %s"
                       (Path.string_of_root file)
                       (Path.string_of_root new_file));
      (System.make_dirp new_file);
      (Json.to_file new_file json)
    )
    files


let mutate_libconfig (libs : (string * old_library) list) : unit =
  let new_libs =
    List.map (fun (library, lib) ->
        {name' = lib.name;
         root' = Path.mk_root lib.root;
         libdata' = {version = "3.0";
                    doc_type = (Library.doc_type_of_string lib.doc_type)
      }})
      libs
  in (write_libconfig new_libs)
  
      
let mutate_db (libs : (string * old_library) list) : unit =
  List.iter (fun (library, lib) ->
      (mutate_library ~library (Path.mk_root lib.root)))
    libs
    

(* Assume 3.0 environment does not already exist,
 * and 2.X environment exists *)
let update_2_to_3_0 () : unit =
  Log.push(Format.sprintf "Upgrading library from version 2.X to 3.0");
  let version,libs = read_libconfig() in
  let version = float_of_string version in
  if version < 2.1 then
    raise (CannotMigrate("Cannot migrate library version < 2.1 from this version of Doculib.\nUse the previous version of doculib to upgrade library to 2.1, then try again."));
  (* otherwise version must be 2.1 *)
  (mutate_libconfig libs);
  (mutate_db libs)

  
let init () : unit =
  if (System.file_exists libconfig_3) then
    ()
  else if (System.file_exists libconfig_2) then
    (update_2_to_3_0 ())
  else
    (* libconfig will be created *)
    ()
