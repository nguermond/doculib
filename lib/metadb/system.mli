(** This is a wrapper for the standard {{: https://v2.ocaml.org/api/Sys.html} [Sys]} library. *)


exception OSError of string
exception InternalError of string
exception NotADirectory of Path.root
                         
(** Try to open file with the system default program. 
    This will try the unix command [xdg-open] followed by [open] upon failure.
    Raises {!OSError} if both fail *)
val xopen : string -> unit

(** Wrapper for {!xopen} taking {!Path.root} as input *)
val open_file : Path.root -> unit

(** Wrapper for {!xopen} for urls *)
val open_url : string -> unit
  
(** Get files recursively in a directory. 
    Optional [hidden] argument is false if {{!Path.hidden} hidden} files should be ignored, which is the default behavior.
    Raises {!NotADirectory} if path is not a directory.
    Raises [Sys.Sys_error] if no such directory exists *)
val get_files : ?hidden:bool -> Path.root -> Path.root Seq.t

(** Recursively remove directory. 
    Raises {!NotADirectory} if path is not a directory. 
    Raises [Sys.Sys_error] if no such directory exists *)
val rmdir : Path.root -> unit

(** Remove a file. Raises [Sys.Sys_error] if file is a directory or does not exist *)
val remove : Path.root -> unit

(** Recursively create directories but ignore the {{!Path.get_leaf} leaf}.
    For example, [make_dirp "/path/to/file.txt"] creates the directories "/path" and "/path/to" if they do not exist. *)
val make_dirp : Path.root -> unit

(** Same as {!make_dirp} but creates a directory for the leaf *)
val make_dirp_leaf : Path.root -> unit

(** This is a wrapper for the {{: https://gildor478.github.io/ocaml-fileutils/} [fileutils]} library move command *)
val move : Path.root -> Path.root -> unit

(** Returns true if file or directory exists *)
val file_exists : Path.root -> bool

(** Returns true if directory is empty. Raises {!NotADirectory} if path is not a directory and [Sys.Sys_error] if directory does not exist. *)
val empty_dir : Path.root -> bool

(** Returns value of environment variable *)
val getenv_opt : string -> string option
