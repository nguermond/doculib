
type t = Yojson.Basic.t

exception ParsingFailure of string
                          
val get : string -> t -> t option

val to_list : t -> t list

val to_int : t -> int

val to_string : t -> string

val to_bool : t -> bool
  
val pretty_to_string : ?std:bool -> t -> string

val from_string : ?buf:Bi_outbuf.t -> ?fname:string -> ?lnum:int -> string -> t
                       
val raise_opt : string -> ([> `Null] as 'a) option -> 'a

val default : ([> `Null] as 'a) -> 'a option -> 'a

val to_file : ?len:int -> ?std:bool -> string -> t -> unit

val from_file : ?buf:Bi_outbuf.t -> ?fname:string -> ?lnum:int -> string -> t

val add_entry : string -> t -> t -> t
val remove_entry : string -> t -> t
