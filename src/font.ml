

let calc_font_height ~(widget : GObj.widget) ?(ypad = 0) (num_lines : int) : int =
  let metrics = (widget#misc#pango_context : GPango.context)#get_metrics () in
  let ascent = metrics#ascent in
  let descent = metrics#descent in
  GPango.to_pixels((2 * (GPango.from_pixels ypad)) + (num_lines * (ascent + descent)))
