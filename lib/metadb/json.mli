(* open Yojson.Basic *)
type t = Yojson.Basic.t

exception ParsingFailure of string

val write_string : ?buf:Buffer.t -> ?len:int -> ?suf:string -> ?std:bool -> t -> string
  
val get : string -> t -> t option

val to_list : t -> t list

val to_int : t -> int

val to_string : t -> string

val to_bool : t -> bool
  
val pretty_to_string : ?std:bool -> t -> string

val from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
                       
val raise_opt : string -> ([> `Null] as 'a) option -> 'a

val default : ([> `Null] as 'a) -> 'a option -> 'a

val get_err : string -> t -> t
  
(* val to_file : ?len:int -> ?std:bool -> ?suf:string -> string -> t -> unit
 * 
 * val from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t *)

val to_file : Path.root -> t -> unit
val from_file : Path.root -> t
  
val add_entry : string -> t -> t -> t
val remove_entry : string -> t -> t
