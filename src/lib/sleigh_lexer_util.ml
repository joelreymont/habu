type lexer_type =
  | Regular
  | Display

let lexer_type = ref Regular
let display_lexer_on () = lexer_type := Display
let display_lexer_off () = lexer_type := Regular
let semi_is_join = ref false

