{
open Util
open Parser

module KeywordTable =
  Map.Make(struct
    type t = string
    let compare a b =
      String.(compare (lowercase_ascii a) (lowercase_ascii b))
  end)

let keyword_table =
  KeywordTable.of_seq @@ List.to_seq [
      "is", RES_IS;
      "if", RES_IF;
      "ram_space", RES_RAM_SPACE;
      "rom_space", RES_ROM_SPACE;
      "register_space", RES_REGISTER_SPACE;
      "register", RES_REGISTER;
      "alignment", KEY_ALIGNMENT;
      "attach", KEY_ATTACH;
      "big", KEY_BIG;
      "build", KEY_BUILD;
      "call", KEY_CALL;
      "dec", KEY_DEC;
      "default", KEY_DEFAULT;
      "define", KEY_DEFINE;
      "endian", KEY_ENDIAN;
      "export", KEY_EXPORT;
      "goto", KEY_GOTO;
      "hex", KEY_HEX;
      "little", KEY_LITTLE;
      "local", KEY_LOCAL;
      "macro", KEY_MACRO;
      "offset", KEY_OFFSET;
      "pcodeop", KEY_PCODEOP;
      "return", KEY_RETURN;
      "signed", KEY_SIGNED;
      "size", KEY_SIZE;
      "space", KEY_SPACE;
      "token", KEY_TOKEN;
      "type", KEY_TYPE;
      "unimpl", KEY_UNIMPL;
      "variables", KEY_VARIABLES;
      "wordsize", KEY_WORDSIZE;
    ]

let expr_keyword_table =
  KeywordTable.of_seq @@ List.to_seq [
      "if", RES_IF;
      "build", KEY_BUILD;
      "call", KEY_CALL;
      "export", KEY_EXPORT;
      "goto", KEY_GOTO;
      "local", KEY_LOCAL;
      "return", KEY_RETURN;
      "unimpl", KEY_UNIMPL;
    ]

let lexing_error lexbuf msg =
  let line, column = Util.get_lexing_position lexbuf in
  raise (Syntax_error (Some (line, column), msg))

let dummy_lexer _ = EOF 

let regular_lexer = ref dummy_lexer
let display_lexer = ref dummy_lexer

let token lexbuf = 
  match get_lexer_state () with
    | Regular -> !regular_lexer lexbuf
    | Display -> !display_lexer lexbuf
}

let alphaup = ['a'-'z' 'A'-'Z' '_' '.']
let digit = ['0'-'9']
let ident = alphaup (alphaup | digit)*
let hexdigit = digit | ['a'-'f' 'A'-'F']
let bindigit = ['0' '1']
let decimal = digit+
let hex = "0x" hexdigit+
let binary = "0b" bindigit+
let text = [^ ' ' 'a'-'z' 'A'-'Z' '_' '.'] [^ ' ']*

rule regular_token = parse
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| ['\t' '\r' ' ']+ { token lexbuf }
| "..."  { ELLIPSIS }
| '{'    { LBRACE }
| '}'    { RBRACE }
| '['    { LBRACKET }
| ']'    { RBRACKET }
| '('    { LPAREN }
| ')'    { RPAREN }
| ':'    { COLON }
| ','    { COMMA }
| '!'    { BANG }
| '~'    { TILDE }
| ';'    { if !semi_is_join then JOIN else SEMI } 
| '='    { ASSIGN }
| '<'    { LESS_THAN }
| '>'    { GREATER_THAN }
| "=="   { EQUAL }
| "!="   { NOT_EQUAL }
| "<="   { LESS_EQUAL }
| ">="   { GREATER_EQUAL }
| "||"   { OR }
| "&&"   { AND }
| "^^"   { XOR }
| '|'    { PIPE }
| '&'    { AMPERSAND }
| '^'    { CARET }
| "<<"   { SHIFT_LEFT }
| ">>"   { SHIFT_RIGHT }
| '+'    { PLUS }
| '-'    { MINUS }
| '*'    { STAR }
| '/'    { SLASH }
| '%'    { PERCENT }
| "$or"  { SPEC_OR }
| "$and" { SPEC_AND }
| "$xor" { SPEC_XOR }
| "f<"   { FLOAT_LESS_THAN }
| "f>"   { FLOAT_GREATER_THAN }
| "f=="  { FLOAT_EQUAL }
| "f!="  { FLOAT_NOT_EQUAL }
| "f<="  { FLOAT_LESS_EQUAL }
| "f>="  { FLOAT_GREATER_EQUAL }
| "f+"   { FLOAT_PLUS }
| "f-"   { FLOAT_MINUS }
| "f*"   { FLOAT_MUL }
| "f/"   { FLOAT_DIV }
| "s<"   { SIGNED_LESS_THAN }
| "s>"   { SIGNED_GREATER_THAN }
| "s<="  { SIGNED_LESS_EQUAL }
| "s>="  { SIGNED_GREATER_EQUAL }
| "s<<"  { SIGNED_SHIFT_LEFT }
| "s>>"  { SIGNED_SHIFT_RIGHT }
| "s/"   { SIGNED_DIV }
| "s%"   { SIGNED_MOD }
| decimal as i { DEC_INT (int_of_string i) }
| binary as i { BIN_INT (int_of_string i) }
| hex as i { HEX_INT (int_of_string i) }
| ident as s
  {
    let table = if !expr_parser then begin
      expr_keyword_table
    end else begin
      keyword_table
    end in
    try KeywordTable.find s table
    with Not_found -> ID s
  }
| '#' { comments lexbuf }
| eof                 { EOF }
| _ as bad_char
  {
    lexing_error lexbuf (Printf.sprintf "Unexpected character \'%c\'" bad_char)
  }

and display_token = parse
| '\n'                { Lexing.new_line lexbuf; token lexbuf }
| ['\t' '\r']+        { token lexbuf }
| ' '+                { if !skip_whitespace then token lexbuf else SPACE }
| ['i' 'I'] ['s' 'S'] { RES_IS }
| ident as s          { ID s }
| text as s           { TEXT s }
| eof                 { EOF }
| _ as bad_char
  {
    lexing_error lexbuf (Printf.sprintf "Unexpected character \'%c\'" bad_char)
  }

and comments = parse
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { comments lexbuf }
| eof  {  raise End_of_file }

{
  regular_lexer := regular_token;
  display_lexer := display_token;
}
