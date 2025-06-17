open Error
module LU = MenhirLib.LexerUtil
module Lexer = Sleigh_lexer
module Parser = Sleigh_parser

let ( let* ) = Result.bind

(* [show text (pos1, pos2)] displays a range of the input text [text]
    delimited by the positions [pos1] and [pos2]. *)
let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 (* max width 43 *)
;;

let parse _text lexbuf parser =
  (* Keep track of the last two tokens in a buffer. *)
  let _buffer, lexer = MenhirLib.ErrorReports.wrap Lexer.token in
  (* and lexbuf = Lexing.from_string text in *)
  parser lexer lexbuf
;;

let parse_file filename =
  let text, lexbuf = LU.read filename in
  parse text lexbuf Parser.grammar
;;
