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

(* This replaces 
 *   GTree.cell_renderer_text.set_fixed_height_from_font
 * which is broken *)
let calc_font_height ~(widget : GObj.widget) ?(ypad = 0) (num_lines : int) : int =
  let metrics = (widget#misc#pango_context : GPango.context)#get_metrics () in
  let ascent = metrics#ascent in
  let descent = metrics#descent in
  GPango.to_pixels((2 * (GPango.from_pixels ypad)) + (num_lines * (ascent + descent)))

(* ampersands must be escaped for Pango *)
let pango_quote (str : string) : string =
    Str.global_replace (Str.regexp (Str.quote "&")) "&amp;" str
                  
