type lexer_type =
  | Regular
  | Display

let lexer_type = ref Regular
let display_lexer_on () = lexer_type := Display
let display_lexer_off () = lexer_type := Regular
let get_lexer_type () = !lexer_type
let semi_is_join = ref false

exception Syntax_error of ((int * int) option * string)

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  line_number, column
;;

let gensym prefix =
  let count = ref (-1) in
  fun () ->
    incr count;
    prefix ^ string_of_int !count
;;
