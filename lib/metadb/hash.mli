(** This is a wrapper for the standard {{: https://v2.ocaml.org/api/Digest.html} [Digest]} library. *)


(** Type of hashes *)
type t

(** Take the MD5 hash of a file *)
val hash_file : Path.root -> t

(** Returns string representation of hash, in hexadecimal *)
val to_string : t -> string

(** Parse hash from hexadecimal string *)  
val of_string : string -> t

(** Equality of hashes *)
val equal : t -> t -> bool

(** Wrapper for [Digest.compare] *)
val compare : t -> t -> int
