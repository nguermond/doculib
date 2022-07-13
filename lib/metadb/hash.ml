(******************************************************************************)
(* Metadb                                                                     *)
(* Copyright (C) 2022 Nathan Guermond                                         *)
(*                                                                            *)
(* This program is free software: you can redistribute it and/or modify it    *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation, either version 3 of the License, or (at your option)  *)
(* any later version.                                                         *)
(*                                                                            *)
(* This program is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *)
(* for more details.                                                          *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License along    *)
(* with this program. If not, see <https://www.gnu.org/licenses/>.            *)
(*                                                                            *)
(******************************************************************************)

module Path = Path

type t = Digest.t

let hash_file (file : Path.root) : string =
  (Digest.file (Path.string_of_root file))


let to_string : t -> string = Digest.to_hex
let of_string : string -> t = Digest.from_hex

let equal : t -> t -> bool = Digest.equal

let compare : t -> t -> int = Digest.compare
