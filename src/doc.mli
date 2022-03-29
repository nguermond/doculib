
type t = {star : bool;
          title : string;
          authors : string list;
          doi : string;
          isbn : string;
          year : string;
          tags : string list;
          path : string;
          doc_type : string;
          hash : string;
         }

type attribute =  Star of bool
                | Title of string
                | Authors of string list
                | Doi of string
                | Isbn of string
                | Year of string
                | Tags of string list
                | Path of string
                | DocType of string
                | Hash of string
  
val set_attribute : string -> string -> attribute
    
val pp_doc : Format.formatter -> t -> unit
val edit_document : attribute -> t -> t

val doc_to_json : t -> Json.t
val json_to_doc : string -> Json.t -> t
val make_doc_from_file : string -> string -> string -> t