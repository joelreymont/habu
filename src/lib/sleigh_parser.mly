%{
open Tag
open Sleigh_tree
open Sleigh_lexer_util

(* Prevent the parser from depending on the library *)
module Habu = struct end

%}

%token <int> DEC_INT
%token <int> BIN_INT
%token <int> HEX_INT
%token <string> ID
%token <string> TEXT
%token RES_IS
%token RES_IF 
%token RES_REGISTER
%token KEY_ALIGNMENT
%token KEY_ATTACH
%token KEY_BIG
%token KEY_DEFAULT
%token KEY_LITTLE
%token KEY_BUILD
%token KEY_CALL
%token KEY_DEC
%token KEY_DEFINE
%token KEY_ENDIAN
%token KEY_EXPORT
%token KEY_GOTO
%token KEY_HEX
%token KEY_LOCAL
%token KEY_MACRO
%token KEY_PCODEOP
%token KEY_RETURN
%token KEY_SIGNED
%token KEY_SPACE
%token KEY_TOKEN
%token KEY_UNIMPL
%token KEY_VARIABLES
%token ELLIPSIS
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token COLON
%token COMMA
%token BANG
%token TILDE
%token SEMI
%token ASSIGN
%token LESS_THAN
%token GREATER_THAN
%token EQUAL
%token NOT_EQUAL
%token LESS_EQUAL
%token GREATER_EQUAL
%token OR
%token AND
%token XOR
%token PIPE
%token AMPERSAND
%token CARET
%token SHIFT_LEFT
%token SHIFT_RIGHT
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT
%token SPEC_OR
%token SPEC_AND
%token SPEC_XOR
%token FLOAT_LESS_THAN
%token FLOAT_GREATER_THAN
%token FLOAT_EQUAL
%token FLOAT_NOT_EQUAL
%token FLOAT_LESS_EQUAL
%token FLOAT_GREATER_EQUAL
%token FLOAT_PLUS
%token FLOAT_MINUS
%token FLOAT_MUL
%token FLOAT_DIV
%token SIGNED_LESS_THAN
%token SIGNED_GREATER_THAN
%token SIGNED_LESS_EQUAL
%token SIGNED_GREATER_EQUAL
%token SIGNED_SHIFT_LEFT
%token SIGNED_SHIFT_RIGHT
%token SIGNED_DIV
%token SIGNED_MOD
%token SPACE
%token EOF

%left PIPE
%left AMPERSAND
%left OR SPEC_OR
%left CARET XOR SPEC_XOR
%left AND SPEC_AND 
%nonassoc EQUAL NOT_EQUAL FLOAT_EQUAL FLOAT_NOT_EQUAL
%nonassoc LESS_THAN GREATER_THAN LESS_EQUAL GREATER_EQUAL
          SIGNED_LESS_THAN SIGNED_GREATER_THAN SIGNED_LESS_EQUAL
		  SIGNED_GREATER_EQUAL FLOAT_LESS_THAN FLOAT_GREATER_THAN
		  FLOAT_LESS_EQUAL FLOAT_GREATER_EQUAL
%left SHIFT_LEFT SHIFT_RIGHT SIGNED_SHIFT_RIGHT SIGNED_SHIFT_LEFT
%left PLUS MINUS FLOAT_MINUS FLOAT_PLUS
%left STAR SLASH PERCENT FLOAT_DIV FLOAT_MUL SIGNED_DIV SIGNED_MOD
%nonassoc UNARY

%start <Sleigh_tree.t> grammar
%%

grammar:
	| endian_definition other_definition* EOF
		{ $1 :: $2 }
	| e = located(error)
		{ error (tag e) "Syntax error" }

other_definition:
	| definition SEMI { $1 }
	| constructor     { Definition.Constructor $1 }
	| macro           { Definition.Macro $1 }

endian_definition:
	KEY_DEFINE KEY_ENDIAN ASSIGN located(endian) SEMI { Definition.Endian $4 } 

endian:
	| KEY_BIG    { Endian.Big }
	| KEY_LITTLE { Endian.Little }

definition:
	| align_definition          { Definition.Alignment $1 }
	| space_definition          { Definition.Space $1 }
	| varnode_definition        { Definition.Varnode $1 }
	| token_definition          { Definition.Token $1 }
	| varnode_attach_definition { Definition.Varnode_attach $1 }
	| pcodeop_definition        { Definition.Pcode_op $1 }
	(*
	| context_definition      { Context $1 }
	| bitrange_definition     { Bit_range $1 }
	| value_attach_definition { Value_attach $1 }
	| name_attach_definition  { Name_attach $1 }
	*)

align_definition:
	KEY_DEFINE KEY_ALIGNMENT ASSIGN constant { $4 }
	
space_definition:
	KEY_DEFINE KEY_SPACE space_name space_mod+
		{ Space.make ~id:$3 ~mods:$4 }

space_mod:
	| id ASSIGN id       { Space.make_kind $1 $3 }
	| id ASSIGN constant { Space.make_size $1 $3 }
	| space_is_default   { Space.make_default $1 }

space_name:
	| RES_REGISTER { with_pos Position.(lex_join $startpos $endpos) "register" }
	| id           { $1 }

varnode_definition:
	varnode_start RES_REGISTER varnode_mod+ LBRACKET id+ RBRACKET
		{ Varnode.make ~pos:$1 ~mods:$3 ~registers:$5 }

varnode_start:
	KEY_DEFINE { Position.with_poss $startpos $endpos () }

varnode_mod:
	id ASSIGN constant { Varnode.make_mod $1 $3 }

token_definition:
	KEY_DEFINE KEY_TOKEN id LPAREN constant RPAREN token_field+
		{ Token.{id = $3; bit_size = $5; fields = $7} }

varnode_attach_definition:
	varnode_attach_start KEY_VARIABLES LBRACKET id+ RBRACKET LBRACKET id+ RBRACKET
		{ Varnode_attach.make ~pos:$1 ~fields:$4 ~registers:$7 }

varnode_attach_start:
	KEY_ATTACH { Position.with_poss $startpos $endpos () }

pcodeop_definition:
	KEY_DEFINE KEY_PCODEOP id { $3 }

constructor:
	| ctr_name COLON display pattern context constructor_body
		{ Constructor.{id = $1; display = $3; pattern = $4; context = $5; body = $6} }
	| COLON ctr_name SPACE display pattern context constructor_body
		{ Constructor.{id = $2; display = $4; pattern = $5; context = $6; body = $7} }
	
ctr_name:
	id
	midrule(display_lexer_on)
		{ $1 }

display:
	mnemonic output { Display.{mnemonic = $1; output = $2} }

mnemonic:
	display_piece* SPACE { $1 }

output:
	| output_piece* midrule(display_lexer_off) RES_IS { $1 }

display_piece:
	| CARET { Display.Caret }
	| id    { Display.Id $1 }
	| text  { Display.Text $1 }

output_piece:
	| CARET { Display.Caret }
	| SPACE { Display.Whitespace }
	| id    { Display.Id $1 }
	| text  { Display.Text $1 }

pattern:
	separated_list(SEMI, pattern_expr) { $1 }

context:
	| LBRACKET context_statement* RBRACKET { $2 }
	|                                      { [] }

constructor_body:
	| LBRACE semantic_body RBRACE     { $2 }
	|                      KEY_UNIMPL { [] }

context_statement: 
	| assignment SEMI { $1 }
	| funcall SEMI    { $1 }

pattern_expr: 
	| pattern_expr pattern_op pattern_expr { Pattern.Binary ($1, $2, $3) }
	| ELLIPSIS pattern_atomic              { Pattern.Align_right $2 }
	| pattern_atomic ELLIPSIS              { Pattern.Align_left $1 }
	| pattern_atomic                       { $1 }
	 
%inline pattern_op:
	| PIPE      { Pattern.OR }
	| AMPERSAND { Pattern.AND }

pattern_atomic:
	| condition                  { Pattern.Constraint $1 }
	| LPAREN pattern_expr RPAREN { Pattern.Paren $2 }

condition:
	| id                             { Symbol $1 }
	| id condition_op condition_expr { Condition ($1, $2, $3) }

condition_expr:
	| condition_expr condition_expr_op condition_expr    { Expr.Binary ($2, $1, $3) }
	| condition_expr_unary_op condition_expr %prec UNARY { Expr.Unary ($1, $2) }
	| id                                                 { Expr.Id $1 }
	| constant                                           { Expr.Int $1 }
	| LPAREN expr = condition_expr RPAREN                { Expr.Paren expr }

semantic_body:
	statement* { $1 }
	
macro:
	KEY_MACRO id LPAREN arg_names RPAREN macro_body
		{ Macro.{id = $2; args = $4; body = $6} }

macro_body:
	| LBRACE semantic_body RBRACE { $2 }

statement:
	| label          { Statement.Label $1 }
	| non_label SEMI { $1 }
	
non_label: 
	| assignment  { $1 }
	| declaration { $1 }
	| funcall     { $1 }
	| build       { $1 }
	| goto        { $1 }
	| branch      { $1 }
	| call        { $1 }
	| export      { $1 }
	| return      { $1 }

assignment:
	| KEY_LOCAL lvalue ASSIGN expr { Statement.Assign ($2, $4) }
	| lvalue ASSIGN expr           { Statement.Assign ($1, $3) }

declaration:
	| KEY_LOCAL id COLON constant { Statement.Declare ($2, Some $4) }
	| KEY_LOCAL id                { Statement.Declare ($2, None) }

build:
	| KEY_BUILD id { Statement.Build $2 }

funcall:
	| id LPAREN args RPAREN { Statement.Fun_call ($1, $3) }

goto:
	| KEY_GOTO jump_target { Statement.Goto $2 }

branch:
	| RES_IF expr KEY_GOTO jump_target { Statement.Branch ($2, $4) }

call:
	| KEY_CALL jump_target { Statement.Call $2 }
	
export:
	| KEY_EXPORT sized_export { Statement.Export $2 }
	| KEY_EXPORT varnode      { Statement.Export $2 }

return:
	| KEY_RETURN LBRACKET jump_target RBRACKET { Statement.Return $3 }

label:
	| LESS_THAN id GREATER_THAN { $2 }

jump_target:
	| constant LBRACKET id RBRACKET { Jump_target.Fixed ($1, Some $3) }
	| constant                      { Jump_target.Fixed ($1, None) }
	| id                            { Jump_target.Direct $1 }
	| LBRACKET expr RBRACKET        { Jump_target.Indirect $2 }
	| label                         { Jump_target.Label $1 }

lvalue:
	| bitrange            { $1 }
	| id COLON constant   { Expr.Sized (Expr.Id $1, $3) }
	| id                  { Expr.Id $1 }
	| sized_pointer(expr) { $1 }
	
expr:
	| expr expr_op expr         { Expr.Binary ($2, $1, $3) }
	| expr_unary_op expr_unary  { Expr.Unary ($1, $2) }
	| sized_pointer(expr_unary) { $1 }
	| expr_unary                { $1 }

expr_unary:
	| expr_funcall { $1 }
	| expr_term    { $1 }

expr_term:
	| LPAREN expr RPAREN { Expr.Paren $2 }
	| varnode            { $1 }
	| bitrange           { $1 }

expr_funcall:
	id LPAREN args RPAREN { Expr.Fun_call ($1, $3) }

varnode:
	| constant                { Expr.Int $1 }
	| id                      { Expr.Id $1 }
	| id COLON constant       { Expr.Sized (Expr.Id $1, $3) }
	| constant COLON constant { Expr.Sized (Expr.Int $1, $3) }

bitrange:
	id LBRACKET constant COMMA constant RBRACKET
		{ Expr.Bit_range ($1, $3, $5) }

sized_export:
	| sized_pointer(id_expr) { $1 }

id_expr:
	| id { Expr.Id $1 }

%inline sized_pointer(X):
	| STAR LBRACKET id RBRACKET COLON constant X { Expr.Sized (Expr.Pointer ($7, Some $3), $6) }
	| STAR LBRACKET id RBRACKET X                { Expr.Pointer ($5, Some $3) }
	| STAR COLON constant X                      { Expr.Sized (Expr.Pointer ($4, None), $3) }
	| STAR X                                     { Expr.Pointer ($2, None) }

args:
	separated_list(COMMA, expr) { $1 }

arg_names:
	separated_list(COMMA, id) { $1 }

%inline condition_op:
	| ASSIGN        { Pattern.EQ }
	| NOT_EQUAL     { Pattern.NE }
	| LESS_THAN     { Pattern.LT}
	| LESS_EQUAL    { Pattern.LE }
	| GREATER_THAN  { Pattern.GT }
	| GREATER_EQUAL { Pattern.GE }

%inline condition_expr_op:
	| SPEC_OR     { Expr.OR }
	| SPEC_AND    { Expr.AND }
	| SPEC_XOR    { Expr.XOR }
	| SHIFT_LEFT  { Expr.LSHIFT }
	| SHIFT_RIGHT { Expr.RSHIFT }
	| PLUS        { Expr.PLUS }
	| MINUS       { Expr.MINUS }

condition_expr_unary_op:
	| MINUS { Expr.NEG }
	| TILDE { Expr.INV }

expr_unary_op:
	| MINUS { Expr.NEG }
	| TILDE { Expr.INV }
	| BANG  { Expr.NOT }

%inline expr_op:
	| OR                   { Expr.BOR }
	| AND                  { Expr.BAND }
	| XOR                  { Expr.BXOR }
	| AMPERSAND            { Expr.AND }
	| PIPE                 { Expr.OR }
	| CARET                { Expr.XOR }
	| EQUAL                { Expr.EQ }
	| NOT_EQUAL            { Expr.NE }
	| LESS_THAN            { Expr.LT }
	| GREATER_THAN         { Expr.GT }
	| LESS_EQUAL           { Expr.LE }
	| GREATER_EQUAL        { Expr.GE }
	| SHIFT_LEFT           { Expr.LSHIFT }
	| SHIFT_RIGHT          { Expr.RSHIFT }
	| PLUS                 { Expr.PLUS }
	| MINUS                { Expr.MINUS }
	| STAR                 { Expr.MUL }
	| SLASH                { Expr.DIV }
	| PERCENT              { Expr.MOD }
	| SIGNED_DIV           { Expr.SDIV }
	| SIGNED_MOD           { Expr.SMOD }
	| SIGNED_SHIFT_LEFT    { Expr.SLSHIFT }
	| SIGNED_SHIFT_RIGHT   { Expr.SRSHIFT }
	| SIGNED_GREATER_EQUAL { Expr.SGE }
	| SIGNED_LESS_EQUAL    { Expr.SLE }
	| SIGNED_GREATER_THAN  { Expr.SGT }
	| SIGNED_LESS_THAN     { Expr.SLT }
	| FLOAT_EQUAL          { Expr.FEQ }
	| FLOAT_NOT_EQUAL      { Expr.FNE }
	| FLOAT_DIV            { Expr.FDIV }
	| FLOAT_MUL            { Expr.FMUL }
	| FLOAT_MINUS          { Expr.FMINUS }
	| FLOAT_PLUS           { Expr.FPLUS }
    | FLOAT_GREATER_EQUAL  { Expr.FGE }
	| FLOAT_GREATER_THAN   { Expr.FGT }
    | FLOAT_LESS_EQUAL     { Expr.FLE }
	| FLOAT_LESS_THAN      { Expr.FLT }

token_field:
	id ASSIGN LPAREN constant COMMA constant RPAREN token_field_mod*
		{ Token_field.make ~id:$1 ~start_bit:$4 ~end_bit:$6 ~mods:$8 }

token_field_mod:
	| signed { Token_field.Signed $1 }
	| hex    { Token_field.Hex $1 }

space_is_default:
	| KEY_DEFAULT { true }

id:
	located(ID) { $1 }

text:
	located(TEXT) { $1 }

%inline located(X):
	x=X { with_pos Position.(lex_join $startpos $endpos) x }

constant:
	| located(integer) { $1 }

integer:
	| DEC_INT { $1 }
	| HEX_INT { $1 }
	| BIN_INT { $1 }

signed:
	| KEY_SIGNED { true }

hex:
	| KEY_HEX { true }
	| KEY_DEC { false }

display_lexer_on:
	| { display_lexer_on() }

display_lexer_off:
	| { display_lexer_off() }


