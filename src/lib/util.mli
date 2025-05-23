type lexer_state =
  | Regular
  | Display

val get_lexer_state : unit -> lexer_state

val flip_lexer_state : unit -> unit 

val skip_whitespace : bool ref
val semi_is_join: bool ref
val expr_parser: bool ref
val debug : bool ref

exception Syntax_error of ((int * int) option * string)

val get_lexing_position : Lexing.lexbuf -> int * int
