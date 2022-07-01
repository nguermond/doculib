

type t = Digest.t

let hash_file (file : Path.root) : string =
  (Digest.file (Path.string_of_root file))


let to_string : t -> string = Digest.to_hex
let of_string : string -> t = Digest.from_hex

let equal : t -> t -> bool = Digest.equal


let to_int (h : t) : int =
  int_of_string ("0x"^(Digest.to_hex h))
