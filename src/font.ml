
(* let default_font = "11" *)

(* This should be calculated from TreeView widget! *)
let calc_font_height ~(widget : GObj.widget) ?(ypad = 0) (num_lines : int) : int =
  (* let fm = Cairo_pango.Font_map.get_default () in
   * let ctx = Cairo_pango.Font_map.create_context fm in
   * let fd = Pango.Font.from_string default_font in
   * let metrics = Pango.Context.get_metrics ctx fd None in *)
  let metrics =
    (widget#misc#pango_context : GPango.context)#get_metrics () in
  (* let ascent = Pango.Font.get_ascent metrics in
   * let descent = Pango.Font.get_descent metrics in *)
  let ascent = metrics#ascent in
  let descent = metrics#descent in
  GPango.to_pixels((2 * (GPango.from_pixels ypad)) + (num_lines * (ascent + descent)))
