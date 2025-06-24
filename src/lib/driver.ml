open Error
module LU = MenhirLib.LexerUtil
module Lexer = Sleigh_lexer
module Parser = Sleigh_parser

let ( let* ) = Result.bind

let parse text lexbuf parser =
  (* Keep track of the last two tokens in a buffer. *)
  let buffer, lexer = MenhirLib.ErrorReports.wrap Lexer.token in
  (* and lexbuf = Lexing.from_string text in *)
  try Ok (parser lexer lexbuf, text, buffer) with
  | Parser.Error -> Error (fail text buffer)
  | Error.Error (poss, msg) ->
    output_string stderr (Error.print_error poss msg);
    Error (fail text buffer)
;;

let parse_file filename =
  let text, lexbuf = LU.read filename in
  parse text lexbuf Parser.grammar
;;
