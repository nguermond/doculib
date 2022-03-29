
let default_font = "11"

let calc_font_height ?(ypad = 0) (num_lines : int) : int =
  let fm = Cairo_pango.Font_map.get_default () in
  let ctx = Cairo_pango.Font_map.create_context fm in
  let fd = Pango.Font.from_string default_font in
  let metrics = Pango.Context.get_metrics ctx fd None in
  let ascent = Pango.Font.get_ascent metrics in
  let descent = Pango.Font.get_descent metrics in
  GPango.to_pixels((1 * (GPango.from_pixels ypad)) + (num_lines * (ascent + descent)))
