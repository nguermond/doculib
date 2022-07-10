(** A minimal library for manipulating typed representations of paths *)


exception InvalidRootType of string
exception InvalidRelType of string
exception InvalidNameType of string

                           
(** A root path has the form ["/some/.../path"] *)
type root

(** A relative path has the form ["some/.../path"] *)
type rel

(** A name is the name of a file or directory, which cannot contain the character ['/'] *)
type name

(** Debugging mode. Debugging mode is set to false by default. *)
val debug : bool ref

(** Constructor for creating a root path, if {!debug} is set to true, [mk_root] will check that input is well-formed. 
    Raise [InvalidRootType] otherwise *)
val mk_root : string -> root

(** Constructor for creating a relative path, if {!debug} is set to true, [mk_rel] will check that input is well-formed.
    Raise [InvalidRelType] otherwise *)  
val mk_rel : string -> rel

(** Constructor for creating a name, if {!debug} is set to true, [mk_name] will check that input is well-formed.
    Raise [InvalidNameType] otherwise *)
val mk_name : string -> name

(** Convert a root path to a string *)
val string_of_root : root -> string

(** Convert a relative path to a string *)
val string_of_rel : rel -> string

(** Convert a name to a string *)
val string_of_name : name -> string

(** [split path] splits a relative path into a list of names *)
val split : rel -> name list

(** [merge root path] produces the path [root'/'path] *)
val merge : root -> rel -> root

(** [merge root names] is the same as [merge] but a relative path is given as a list of names *)
val merge_lst : root -> name list -> root

(** [unroot "/some/.../path"] will return [("/", "some/.../path")] *)
val unroot : root -> root * rel

(** [add_file_ext ext path] wiil add the file extension [ext] to the {{!get_leaf} leaf} of [path], that is [path'.'ext] *)
val add_file_ext : string -> root -> root

(** [remove_file_ext ext path] removes the file extension from the file name *)
val remove_file_ext : string -> root -> root

(** Same as [remove_file_ext], but for relative paths *)
val remove_file_ext_rel : string -> rel -> rel

(** [strip_root root path] will drop the initial substring [root] from [path], thus inversing {!merge} *)
val strip_root : root -> root -> rel

(** [get_leaf "/some/.../path/leaf"] returns the leaf of a path, that is [leaf] *)
val get_leaf : root -> name

(** Same as [get_leaf] but for relative paths *)
val get_leaf_rel : rel -> name

(** [drop_leaf "/some/.../path/leaf"] will return ["/some/.../path"] *)
val drop_leaf : root -> root

(** [hidden path] will return true if the {{!get_leaf} leaf} of [path] is a hidden file, that is starts with ['.'] *)
val hidden : root -> bool
