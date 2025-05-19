let parse lexbuf =
  try
    let grammar = Parse.parse lexbuf (Parser.Incremental.grammar lexbuf.lex_curr_p) in
    Ok grammar
  with
  | Util.Syntax_error (pos, err) ->
    (match pos with
     | Some (line, pos) ->
       Error (Printf.sprintf "Syntax error on line %d, character %d: %s" line pos err)
     | None -> Error (Printf.sprintf "Syntax error: %s" err))
;;

let ast_from_channel ic =
  let lexbuf = Lexing.from_channel ic in
  parse lexbuf
;;

let ast_from_file filename =
  let ic = open_in filename in
  let g = ast_from_channel ic in
  let () = close_in ic in
  g
;;

let ast_from_string s =
  let lexbuf = Lexing.from_string s in
  parse lexbuf
;;
