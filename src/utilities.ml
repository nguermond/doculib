(******************************************************************************)
(* DocuLib                                                                    *)
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

exception InternalError of string

module List =
  struct
    include List
    let index (lst : 'a list) (x : 'a) : int option =
      let ind = ref None in
      (List.iteri (fun j y ->
           (if (x = y) && (!ind = None) then ind := (Some j))) lst);
      !ind

    let assoc_index (lst : ('a * 'b) list) (x : 'a) : int option =
      let ind = ref None in
      (List.iteri (fun j (y,_) ->
           (if (x = y) && (!ind = None) then ind := (Some j))) lst);
      !ind

  end
