
exception EnvVarNotSet
exception InvalidDocType
       

   

type doc_type = [`Book | `Document]


val get_library_descriptions : unit -> (string * doc_type) list
                     
val init : unit -> unit
