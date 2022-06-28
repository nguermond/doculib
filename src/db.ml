(* open Lwt.Syntax *)
open Format

exception EnvVarNotSet
exception InvalidDocType
       

   
open Metadb


(* The database is versioned, to prevent data loss upon upgrade. *)
let branches = ["3.0"; "2.1"; "2.0"]
let current_branch = "3.0"

                   
let configdir : Path.root =
  match (Sys.getenv_opt "XDG_CONFIG_HOME") with
  | Some usr_config -> Path.merge_lst (Path.mk_root usr_config)
                         [(Path.mk_name "doculib")]
  | None -> 
     (match (Sys.getenv_opt "HOME") with
      | Some home -> Path.merge (Path.mk_root home)
                       (Path.mk_rel ".config/doculib")
      | None -> raise EnvVarNotSet)

let libconfig : Path.root =
  Path.merge_lst configdir [(Path.mk_name "libraries.json")]

type doc_type = [`Book | `Document]

let string_of_doc_type = function
  | `Book -> "book"
  | `Document -> "document"
               
let doc_type_of_string = function
  | "book" -> `Book
  | "document" -> `Document
  | _ -> raise (InvalidDocType)

module LibData =
  struct
    type t = {
        version : string;
        doc_type : doc_type;
      }
    let to_json (lib : t) : Json.t =
      (`Assoc [("version",`String lib.version);
               ("doc_type", `String (string_of_doc_type lib.doc_type))
      ])
    let from_json (j : Json.t) : t =
      let v = (Json.to_string (Json.get_err "version" j)) in
      let dtype = (doc_type_of_string (Json.to_string (Json.get_err "doc_type" j)))
      in {version = v ; doc_type = dtype}
  end



module Libraries = Make(Doc)(LibData)
                     

 
