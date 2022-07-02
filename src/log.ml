

let log : (string list) ref = ref []
       
let push msg : unit =
  prerr_endline msg;
  log := (msg :: !log)
