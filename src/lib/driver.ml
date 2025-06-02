open Printf
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module Lexer = Sleigh_lexer
module Parser = Sleigh_parser

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)
let show text positions =
  E.extract text positions |> E.sanitize |> E.compress |> E.shorten 20 (* max width 43 *)
;;

let fail text buffer =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Show these three components. *)
  sprintf "%s%s%!" location indication
;;

let parse filename =
  (* Keep track of the last two tokens in a buffer. *)
  let buffer, lexer = MenhirLib.ErrorReports.wrap Lexer.token in
  let text, lexbuf = L.read filename in
  try Ok (Parser.grammar lexer lexbuf) with
  | Parser.Error -> Error (fail text buffer)
  | Util.Syntax_error (pos, err) ->
    (match pos with
     | Some (line, pos) ->
       Error (Printf.sprintf "Syntax error on line %d, character %d: %s" line pos err)
     | None -> Error (Printf.sprintf "Syntax error: %s" err))
;;
