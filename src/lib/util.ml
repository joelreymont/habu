type lexer_state =
  | Regular
  | Display

let lexer_state = ref Regular
let get_lexer_state () = !lexer_state
let debug = ref false

let flip_lexer_state () =
  let state =
    match !lexer_state with
    | Regular ->
      debug := true;
      Display
    | Display -> Regular
  in
  lexer_state := state
;;

let skip_whitespace = ref true
let semi_is_join = ref false
let expr_parser = ref false

exception Syntax_error of ((int * int) option * string)

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  line_number, column
;;
