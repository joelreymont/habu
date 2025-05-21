open Printf
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)
let show text positions =
  E.extract text positions |> E.sanitize |> E.compress |> E.shorten 20 (* max width 43 *)
;;

let fail text buffer (state : int) =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message =
    try Parser_messages.message state with
    | Not_found -> Printf.sprintf "Unknown syntax error (in state %d).\n" state
  in
  let message = sprintf "%d: %s" state message in
  (* Show these three components. *)
  sprintf "%s%s%s%!" location indication message
;;

let parse filename =
  (* Keep track of the last two tokens in a buffer. *)
  let buffer, lexer = MenhirLib.ErrorReports.wrap Lexer.token in
  let text, lexbuf = L.read filename in
  try Ok (Parser.grammar lexer lexbuf) with
  | Parser.Error state -> Error (fail text buffer state)
  | Util.Syntax_error (pos, err) ->
    (match pos with
     | Some (line, pos) ->
       Error (Printf.sprintf "Syntax error on line %d, character %d: %s" line pos err)
     | None -> Error (Printf.sprintf "Syntax error: %s" err))
;;
