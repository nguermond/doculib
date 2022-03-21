
(* In case of change in data format in the database,
 * we need to mutate the entries to the new format.
 *)

open Lwt.Syntax
open Format
open Db
   

exception InternalError of string

(* Version 2.0 -> Version 2.1 *)
module V2_0_to_2_1 =
  struct
    let old_db () = new Db.db "2.0"
    let new_db () = new Db.db "2.1"
               
    let mutate_entry ~(library:string) (doc : Db.doc) : Db.doc =
      let full_path = ((old_db())#get_full_path ~library doc.path) in
      let hash = (Utilities.Sys.hash full_path) in
      { star = doc.star;
        title = doc.title;
        authors = doc.authors;
        doi = doc.doi;
        isbn = doc.isbn;
        year = doc.year;
        tags = doc.tags;
        path = doc.path;
        doc_type = doc.doc_type;
        hash = hash;
      }

    let mutate_library ~library : unit =
      prerr_endline ("Migrating library: "^library);
      let old_docs = (old_db())#get_documents library in
      let new_docs = (List.map (mutate_entry ~library) old_docs) in
      List.iter ((new_db())#add_document ~library) new_docs

    let mutate_db () : unit =
      let libraries = (old_db())#get_libraries() in
      List.iter (fun library -> mutate_library ~library) libraries
      
  end

let migrate_db (old_branch : string) (new_branch : string) =
  match (old_branch,new_branch) with
  | ("2.0", "2.1") -> V2_0_to_2_1.mutate_db()
  | _ -> raise (InternalError "Impossible database migration")


let update_db (version : string) : unit =
  (prerr_endline ("Updating version "^version^" to version "^Db.current_branch));
  (migrate_db version Db.current_branch);
  (Db.set_lib_version Db.current_branch)

  
let init () : unit =
  (if (not (Sys.file_exists configdir)) then
     (prerr_endline "configuration directory does not exist: creating...";
      Sys.mkdir configdir 0o755));
  (if (not (Sys.file_exists datadir)) then
     (prerr_endline "data directory does not exist: creating...";
      Sys.mkdir datadir 0o755));
  (if (not (Sys.file_exists libconfig)) then
     (prerr_endline "configuration file does not exist: creating...";
      init_lib_config ()));
  let version = (get_lib_version ()) in
  (if version <> current_branch then
     (update_db version))
