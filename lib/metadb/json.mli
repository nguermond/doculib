(** This is a wrapper for the {{: https://ocaml-community.github.io/yojson/yojson/index.html} [Yojson]} library *)

(** Type of {{: https://ocaml-community.github.io/yojson/yojson/Yojson/Basic/index.html} Basic} Json values *)
type t = [ 
  | `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list
  ]
       
exception ParsingFailure of string

(** Get value of a key in a Json association list. Returns none if key is not found. Raises {!ParsingFailure} if not an association list *)
val get : string -> t -> t option

(** Same as {!get} but raises {!ParsingFailure} if key is unbound *)  
val get_err : string -> t -> t

(** Add entry to an association list and raise {!ParsingFailure} if not an association list *)
val add_entry : string -> t -> t -> t

(** Remove entry from association list and raise {!ParsingFailure} if not an association list *)
val remove_entry : string -> t -> t

(** Convert a Json list to a list. Raise {!ParsingFailure} if not a list *)
val to_list : t -> t list

(** Convert a Json int to an int. Raise {!ParsingFailure} if not an int *)
val to_int : t -> int

(** Convert Json string to a string. Raise {!ParsingFailure} if not a string *)
val to_string : t -> string

(** Convert Json bool to bool. Raise {!ParsingFailure} if not a bool *)
val to_bool : t -> bool
  

(** Convert {!t} [option] to {!t} and raise {!ParsingFailure} with message specified by the first argument if [None] or [`Null] *)                       
val raise_opt : string -> ([> `Null] as 'a) option -> 'a

(** Convert {!t} [option] to {!t} and return first argument if [None] or [`Null] *)
val default : 'a -> ([> `Null] as 'a) option -> 'a



(** Write Json to string in a compact way *)
val write_string : t -> string

(** Write Json to string in a readable way *)
val pretty_to_string : t -> string

(** Parse Json from string *)
val from_string : string -> t

(** Write Json to file *)
val to_file : Path.root -> t -> unit

(** Read Json from file *)  
val from_file : Path.root -> t
