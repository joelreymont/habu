{
open Sleigh_lexer_util
open Sleigh_parser

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
      "pcodeop", KEY_PCODEOP;
      "return", KEY_RETURN;
      "signed", KEY_SIGNED;
      "space", KEY_SPACE;
      "token", KEY_TOKEN;
      "unimpl", KEY_UNIMPL;
      "variables", KEY_VARIABLES;
    ]

let dummy_lexer _ = EOF 

let regular_lexer = ref dummy_lexer
let display_lexer = ref dummy_lexer

let token lexbuf = 
  match !lexer_type with
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
| binary as i  { BIN_INT (int_of_string i) }
| hex as i     { HEX_INT (int_of_string i) }
| ident as s
  {
    try KeywordTable.find s keyword_table
    with Not_found -> ID s
  }
| '#' { comments lexbuf }
| eof { EOF }
| _   { BAD_CHAR }

and display_token = parse
| '\n'                { Lexing.new_line lexbuf; token lexbuf }
| ['\t' '\r']+        { token lexbuf }
| ' '+                { SPACE }
| ['i' 'I'] ['s' 'S'] { RES_IS }
| '^'                 { CARET }
| ident as s          { ID s }
| text as s           { TEXT s }
| eof                 { EOF }
| _                   { BAD_CHAR }

and comments = parse
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { comments lexbuf }
| eof  {  raise End_of_file }

{
  regular_lexer := regular_token;
  display_lexer := display_token;
}
