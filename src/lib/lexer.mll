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
      "offset", KEY_OFFSET;
      "pcodeop", KEY_PCODEOP;
      "return", KEY_RETURN;
      "signed", KEY_SIGNED;
      "size", KEY_SIZE;
      "space", KEY_SPACE;
      "token", KEY_TOKEN;
      "unimpl", KEY_UNIMPL;
      "variables", KEY_VARIABLES;
      "wordsize", KEY_WORDSIZE;
    ]

let lexing_error lexbuf msg =
  let line, column = Util.get_lexing_position lexbuf in
  raise (Syntax_error (Some (line, column), msg))

}

let alphaup = ['a'- 'z' 'A'-'Z' '_' '.']
let digit = ['0'-'9']
let ident = alphaup (alphaup | digit)*
let hexdigit = digit | ['a'-'f' 'A'-'F']
let bindigit = ['0' '1']
let decimal = digit+
let hex = "0x" hexdigit+
let binary = "0b" bindigit+

rule token = parse
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
| ';'    { SEMI } 
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
| ident as s {
    try KeywordTable.find s keyword_table
    with Not_found -> ID s
  }
| _ as bad_char
{ lexing_error lexbuf (Printf.sprintf "Unexpected character \'%c\'" bad_char) }

and read_double_quoted_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_double_quoted_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_double_quoted_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_double_quoted_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_double_quoted_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_double_quoted_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_double_quoted_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_double_quoted_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_double_quoted_string buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; read_double_quoted_string buf lexbuf }
  | '\n'      { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_double_quoted_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_double_quoted_string buf lexbuf
    }
  | eof { lexing_error lexbuf "Quoted string is missing the closing double quote" }

