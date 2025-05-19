type settings =
  { debug : bool
  ; debug_fun : string -> unit
  }

let default_settings = { debug = false; debug_fun = print_endline }
