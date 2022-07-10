
module Path = Path

type t = Digest.t

let hash_file (file : Path.root) : string =
  (Digest.file (Path.string_of_root file))


let to_string : t -> string = Digest.to_hex
let of_string : string -> t = Digest.from_hex

let equal : t -> t -> bool = Digest.equal

let compare : t -> t -> int = Digest.compare
