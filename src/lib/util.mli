type lexer_type =
  | Regular
  | Display

val display_lexer_on : unit -> unit 
val display_lexer_off : unit -> unit 
val get_lexer_type : unit -> lexer_type

val semi_is_join: bool ref

exception Syntax_error of ((int * int) option * string)

val get_lexing_position : Lexing.lexbuf -> int * int
val gensym : string -> unit -> string 
