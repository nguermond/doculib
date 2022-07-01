

let log : (string list) ref = ref []
       
let push msg : unit =
  log := (msg :: !log)
