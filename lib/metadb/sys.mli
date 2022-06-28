
exception OSError of string
exception InternalError of string

(* Try to open file with default program *)
val xopen : Path.root -> unit

(* Get files recursively *)
val get_files_map : Path.root -> (Path.root -> 'a) -> 'a list

(* Remove directory recursively *)
val rmdir : Path.root -> unit

(* Remove file *)
val remove : Path.root -> unit

(* Recursively make directories (same as (mkdir -p ...)) *)
(* ignore leaf ie. /path/to/file.txt *)
val make_dirp : Path.root -> unit
(* make directory at leaf ie. /path/to/dir *)
val make_dirp_leaf : Path.root -> unit

val move : Path.root -> Path.root -> unit

val file_exists : Path.root -> bool
val empty_dir : Path.root -> bool
