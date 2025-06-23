let exit_flag = ref false
let exit_on_error () = exit_flag := true
let resume_on_error () = exit_flag := false

exception Error of Position.t list * string

let print_error positions msg =
  Printf.sprintf
    "%s%s\n"
    (String.concat
       "\n"
       (List.map (fun p -> Position.string_of_pos p ^ ": ") positions))
    msg
;;

let error_alert positions msg =
  if !exit_flag then (
    output_string stderr (print_error positions msg);
    exit 1
  ) else
    raise (Error (positions, msg))
;;

let global_error = error_alert []
let errorN poss = error_alert poss
let error pos = errorN [ pos ]
let error2 pos1 pos2 = errorN [ pos1; pos2 ]
