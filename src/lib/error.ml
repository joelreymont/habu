open Printf
module L = Lexing
module E = MenhirLib.ErrorReports
module LU = MenhirLib.LexerUtil

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

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)
let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 (* max width 43 *)
;;

let fail text buffer =
  (* Indicate where in the input file the error occurred. *)
  let location = LU.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Show these three components. *)
  sprintf "%s%s%!" location indication
;;
