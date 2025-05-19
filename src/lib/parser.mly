%{
open AST
open Position

let output_parsing_mode = ref false
%}

%token <int> DEC_INT
%token <int> BIN_INT
%token <int> HEX_INT
%token <string> STRING
%token <string> ID
%token RES_IS
%token RES_IF 
%token RES_RAM_SPACE
%token RES_ROM_SPACE
%token RES_REGISTER_SPACE
%token RES_REGISTER
%token KEY_ALIGNMENT
%token KEY_ATTACH
%token KEY_BIG
%token KEY_LITTLE
%token KEY_BUILD
%token KEY_CALL
%token KEY_DEC
%token KEY_DEFAULT
%token KEY_DEFINE
%token KEY_ENDIAN
%token KEY_EXPORT
%token KEY_GOTO
%token KEY_HEX
%token KEY_LOCAL
%token KEY_OFFSET
%token KEY_PCODEOP
%token KEY_RETURN
%token KEY_SIGNED
%token KEY_SIZE
%token KEY_SPACE
%token KEY_TOKEN
%token KEY_UNIMPL
%token KEY_VARIABLES
%token KEY_WORDSIZE
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

%left PLUS MINUS      /* lowest precedence */
%left STAR SLASH      /* medium precedence */
%nonassoc SEMI UMINUS /* highest precedence */

%start <AST.t> grammar
%%

grammar:
	| definitions EOF    { $1 }
	| e = located(error) { Error.error "parsing" e.position "Syntax error." }

identifier:
	located(ID) { $1 }

%inline located(X):
	x=X { Position.with_poss $startpos $endpos x }

definitions:
	endian_definition separated_list(COMMA, definition) { $1 :: $2 } 

endian_definition:
	KEY_DEFINE KEY_ENDIAN ASSIGN located(endian) SEMI { Endian $4 } 

endian:
	| KEY_BIG    { Big }
	| KEY_LITTLE { Little }

definition:
	| align_definition          { $1 }
	| space_definition          { $1 }
	| varnode_definition        { $1 }
	| token_definition          { $1 }
	| varnode_attach_definition { $1 }
	| pcodeop_definition        { $1 }
	| constructor_definition    { $1 }
	(*
	| context_definition { Context $1 }
	| bitrange_definition { BitRange $1 }
	| value_attach_definition { ValueAttach $1 }
	| name_attach_definition { NameAttach $1 }
	*)

align_definition:
	KEY_DEFINE KEY_ALIGNMENT ASSIGN constant { Alignment $4 }
	
space_definition:
	KEY_DEFINE KEY_SPACE identifier  
	located(space_type) size word_size space_is_default
		{ Space { id = $3; kind = $4; size = $5; word_size = $6; is_default = $7 } }

varnode_definition:
	KEY_DEFINE RES_REGISTER size offset LBRACKET spaced_ids RBRACKET
		{ VarNode { offset = $4; size = $3; registers = $6 } }

spaced_ids:
	| separated_list(SPACE, identifier) { $1 }

token_definition:
	KEY_DEFINE KEY_TOKEN identifier LPAREN constant RPAREN token_field+
		{ Token { id = $3; bit_size = $5; fields = $7 } }

varnode_attach_definition:
	KEY_ATTACH KEY_VARIABLES LBRACKET spaced_ids RBRACKET LBRACKET spaced_ids RBRACKET
		{ VarNodeAttach { nodes = $4; values = $7 } }

pcodeop_definition:
	KEY_DEFINE KEY_PCODEOP identifier { PCodeOp $3 }

/*
and constructor =
  { id: identifier;
    display: display;
    bit_pattern: expr option;
    context: context option;
    body: statement list;
  }
*/

constructor_definition:
	identifier? COLON display RES_IS pattern? context constructor_body
		{ Constructor { id = $1; display = $3; pattern = $5; context = $6; body = $7 } }
	
display:
	| mnemonic output  { { mnemonic = $1; output = $2 } }

mnemonic:
	| display_pieces { $1 }

output:
	flip_output_parsing_mode
	pieces = display_pieces
	flip_output_parsing_mode
		{ pieces }

display_pieces:
	display_piece+ { $1 }
	
display_piece:
	| CARET      { Caret }
	| identifier { Id $1 }

pattern:
	pattern_expr { $1 }

context:
	| LBRACKET statements RBRACKET { $2 }
	|                              { [] }

constructor_body:
	| LBRACE statements RBRACE { $2 }
	| KEY_UNIMPL               { [] }

statements:
	separated_list(COMMA, statement) { $1 }

pattern_expr: 
	| pattern_expr pattern_op pattern_expr { Binary ($2, $1, $3) }
	| ELLIPSIS pattern_atomic              { Unary (OpAlignRight, $2) }
	| pattern_atomic ELLIPSIS              { Unary (OpAlignLeft, $1) }
	 
pattern_atomic:
	| condition { $1 }
	| LPAREN pattern_expr RPAREN { Paren $2 }

condition:
	identifier condition_op  condition_expr { Binary ($2, Id $1, $3)}

condition_expr:
	| condition_expr condition_expr_op condition_expr { Binary ($2, $1, $3) }
	| condition_expr_unary_op condition_expr          { Unary ($1, $2) }
	| identifier                                      { Id $1 }
	| constant                                        { Int $1 }
	| LPAREN expr = condition_expr RPAREN             { Paren expr }

statement:
	| separated SEMI { $1 }
	| label          { Label $1 }
	
separated: 
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
	| KEY_LOCAL lvalue ASSIGN expr { Assign ($2, $4) }
	| lvalue ASSIGN expr           { Assign ($1, $3) }

declaration:
	| KEY_LOCAL identifier COLON constant { Declare { id = $2; size = Some $4 } }
	| KEY_LOCAL identifier                { Declare { id = $2; size = None } }

build:
	| KEY_BUILD identifier { Build $2 }

funcall:
	| identifier LPAREN args RPAREN { FunCall ($1, $3) }

goto:
	| KEY_GOTO jump_target { Goto $2 }

branch:
	| RES_IF expr KEY_GOTO jump_target { Branch { condition = $2; target = $4 } }

call:
	| KEY_CALL jump_target { Call $2 }
	
export:
	| KEY_EXPORT sized_export { Export $2 }
	| KEY_EXPORT varnode      { Export $2 }

return:
	| KEY_RETURN LBRACKET jump_target RBRACKET { Return $3 }

label:
	| LESS_THAN identifier GREATER_THAN { $2 }

jump_target:
	| constant LBRACKET identifier RBRACKET { FixedJump ($1, Some $3) }
	| constant                              { FixedJump ($1, None) }
	| identifier                            { DirectJump $1 }
	| LBRACKET expr RBRACKET                { IndirectJump $2 }
	| label                                 { LabelJump $1 }

lvalue:
	| bitrange                  { $1 }
	| identifier COLON constant { Sized { expr = Id $1; size = $3 } }
	| identifier                { Id $1 }
	| sized_pointer(expr)       { $1 }
	
expr:
	| expr expr_op expr  { Binary ($2, $1, $3) }
	| expr_unary_op expr { Unary ($1, $2) }
	| LPAREN expr RPAREN { Paren $2 }
	| identifier         { Id $1 }
	| constant           { Int $1 }

varnode:
	| constant                  { Int $1 }
	| identifier                { Id $1 }
	| identifier COLON constant { Sized { expr = Id $1; size = $3 } }
	/*| integer COLON constant { }*/

bitrange:
	id = identifier LBRACKET start_bit = constant COMMA width = constant RBRACKET
		{ BitRange { id; start_bit; width }}

sized_export:
	| sized_pointer(id_expr) { $1 }

id_expr:
	| identifier { Id $1 }

%inline sized_pointer(X):
	| STAR LBRACKET space = identifier RBRACKET COLON size = constant expr = X
		{ Sized { expr = Pointer { space = Some space; expr };  size } }
	| STAR LBRACKET space = identifier RBRACKET expr = X
		{ Pointer { space = Some space; expr } }
	| STAR COLON size = constant expr = X
		{ Sized { expr = Pointer { space = None; expr };  size } }
	| STAR expr = X
		{ Pointer { space = None; expr } }

args:
	separated_list(COMMA, expr) { $1 }

pattern_op:
	| SEMI      { OpJoin }
	| PIPE      { OpBoolOr }
	| AMPERSAND { OpBoolAnd }

condition_op:
	| ASSIGN        { OpEq }
	| NOT_EQUAL     { OpNe }
	| LESS_THAN     { OpLt }
	| LESS_EQUAL    { OpLe }
	| GREATER_THAN  { OpGt }
	| GREATER_EQUAL { OpGe }

condition_expr_op:
	| SPEC_OR     { OpOr }
	| SPEC_AND    { OpAnd }
	| SPEC_XOR    { OpXor }
	| SHIFT_LEFT  { OpShiftLeft }
	| SHIFT_RIGHT { OpShiftRight }
	| PLUS        { OpPlus }
	| MINUS       { OpMinus }

condition_expr_unary_op:
	| MINUS { OpNeg }
	| TILDE { OpTilde }

expr_unary_op:
	| MINUS { OpNeg }
	| TILDE { OpTilde }
	| BANG  { OpBang }

expr_op:
	| OR                   { OpBoolOr }
	| AND                  { OpBoolAnd }
	| XOR                  { OpBoolXor }
	| AMPERSAND            { OpAnd }
	| PIPE                 { OpOr }
	| CARET                { OpXor }
	| EQUAL                { OpEq }
	| NOT_EQUAL            { OpNe }
	| LESS_THAN            { OpLt }
	| GREATER_THAN         { OpGt }
	| LESS_EQUAL           { OpLe }
	| GREATER_EQUAL        { OpGe }
	| SHIFT_LEFT           { OpShiftLeft }
	| SHIFT_RIGHT          { OpShiftRight }
	| PLUS                 { OpPlus }
	| MINUS                { OpMinus }
	| STAR                 { OpMul }
	| SLASH                { OpDiv }
	| PERCENT              { OpMod }
	| SIGNED_DIV           { OpSignedDiv }
	| SIGNED_MOD           { OpSignedMod }
	| SIGNED_SHIFT_LEFT    { OpSignedShiftLeft }
	| SIGNED_SHIFT_RIGHT   { OpSignedShiftRight }
	| SIGNED_GREATER_EQUAL { OpSignedGe }
	| SIGNED_LESS_EQUAL    { OpSignedLe }
	| SIGNED_GREATER_THAN  { OpSignedGt }
	| SIGNED_LESS_THAN     { OpSignedLt }
	| FLOAT_EQUAL          { OpFloatEq }
	| FLOAT_NOT_EQUAL      { OpFloatNe }
	| FLOAT_DIV            { OpFloatDiv }
	| FLOAT_MUL            { OpFloatMul }
	| FLOAT_MINUS          { OpFloatMinus }
	| FLOAT_PLUS           { OpFloatPlus }
    | FLOAT_GREATER_EQUAL  { OpFloatGe }
	| FLOAT_GREATER_THAN   { OpFloatGt }
    | FLOAT_LESS_EQUAL     { OpFloatLe }
	| FLOAT_LESS_THAN      { OpFloatLt }

token_field:
	id = identifier LPAREN start_bit = constant COMMA end_bit = constant RPAREN
	is_signed = signed is_hex = hex
		{ { id; start_bit; end_bit; is_signed; is_hex } }

space_type:
	| RES_RAM_SPACE      { Ram }
	| RES_ROM_SPACE      { Rom }
	| RES_REGISTER_SPACE { Register }

space_is_default:
	| KEY_DEFAULT { true }
	|             { false }

size:
	KEY_SIZE ASSIGN constant { $3 }

word_size:
	KEY_WORDSIZE ASSIGN constant { $3 }

offset:
	KEY_OFFSET ASSIGN constant { $3 }

constant:
	| located(integer) { $1 }

integer:
	| DEC_INT { $1 }
	| HEX_INT { $1 }
	| BIN_INT { $1 }

signed:
	| KEY_SIGNED { true }
	|            { false }

hex:
	| KEY_HEX { true }
	| KEY_DEC { false }
	|         { false }

flip_output_parsing_mode: { output_parsing_mode := !output_parsing_mode }

