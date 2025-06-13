open Printf
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module Lexer = Sleigh_lexer
module Parser = Sleigh_parser

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
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Show these three components. *)
  sprintf "%s%s%!" location indication
;;

let parse text lexbuf parser =
  (* Keep track of the last two tokens in a buffer. *)
  let buffer, lexer = MenhirLib.ErrorReports.wrap Lexer.token in
  (* and lexbuf = Lexing.from_string text in *)
  try Ok (parser lexer lexbuf) with
  | Parser.Error -> Error (fail text buffer)
  | Error.Error (poss, msg) ->
    output_string stderr (Error.print_error poss msg);
    Error (fail text buffer)
;;

let parse_file filename =
  let text, lexbuf = L.read filename in
  parse text lexbuf Parser.grammar
;;
