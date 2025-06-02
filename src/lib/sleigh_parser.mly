%{
open Sleigh
open Position
open Util
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
%token JOIN
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

%left JOIN
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

%start <Sleigh.t> grammar
%%

grammar:
	| endian_definition other_definition* EOF
		{ $1 :: $2 }
	| e = located(error)
		{ Error.error "parsing" e.position "Syntax error." }

other_definition:
	| definition SEMI { $1 }
	| constructor     { $1 }
	| macro           { $1 }

endian_definition:
	KEY_DEFINE KEY_ENDIAN ASSIGN located(endian) SEMI { Sdef_endian $4 } 

endian:
	| KEY_BIG    { Send_big }
	| KEY_LITTLE { Send_little }

definition:
	| align_definition          { $1 }
	| space_definition          { $1 }
	| varnode_definition        { $1 }
	| token_definition          { $1 }
	| varnode_attach_definition { $1 }
	| pcodeop_definition        { $1 }
	(*
	| context_definition { Context $1 }
	| bitrange_definition { BitRange $1 }
	| value_attach_definition { ValueAttach $1 }
	| name_attach_definition { NameAttach $1 }
	*)

align_definition:
	KEY_DEFINE KEY_ALIGNMENT ASSIGN constant { Sdef_alignment $4 }
	
space_definition:
	KEY_DEFINE KEY_SPACE space_name space_mod+
		{ Sdef_space { ssp_id = $3; ssp_mods = $4 } } 

space_mod:
	| identifier ASSIGN identifier { Sspmod_id ($1, $3) }
	| identifier ASSIGN constant   { Sspmod_int ($1, $3) }
	| space_is_default             { Sspmod_is_default $1 }

space_name:
	| RES_REGISTER { Position.with_poss $startpos $endpos "register" }
	| identifier   { $1 }

varnode_definition:
	KEY_DEFINE RES_REGISTER varnode_mod+ LBRACKET identifiers RBRACKET
		{ Sdef_varnode { svn_mods = $3; svn_registers = $5 } }

varnode_mod:
	identifier ASSIGN constant { ($1, $3) }

token_definition:
	KEY_DEFINE KEY_TOKEN identifier LPAREN constant RPAREN token_field+
		{ Sdef_token { stok_id = $3; stok_bit_size = $5; stok_fields = $7 } }

varnode_attach_definition:
	KEY_ATTACH KEY_VARIABLES LBRACKET identifiers RBRACKET LBRACKET identifiers RBRACKET
		{ Sdef_varnode_attach { svna_nodes = $4; svna_values = $7 } }

pcodeop_definition:
	KEY_DEFINE KEY_PCODEOP identifier { Sdef_pcode_op $3 }

constructor:
	ctr_name COLON display pattern? context constructor_body
		{ Sdef_constructor { sctr_id = $1; sctr_display = $3; sctr_pattern = $4; sctr_context = $5; sctr_body = $6 } }
	
ctr_name:
	identifier?
	midrule(display_lexer_on)
	midrule(semi_is_join)
		{ $1 }

display:
	mnemonic output
		{ { sdsp_mnemonic = $1; sdsp_output = $2 } }

mnemonic:
	display_piece* SPACE
		{ $1 }

output:
	| output_piece* midrule(display_lexer_off) RES_IS { $1 }

display_piece:
	| CARET      { Spc_caret }
	| identifier { Spc_id $1 }
	| text       { Spc_text $1 }

output_piece:
	| CARET      { Spc_caret }
	| SPACE      { Spc_whitespace }
	| identifier { Spc_id $1 }
	| text       { Spc_text $1 }

pattern:
	pattern_expr midrule(semi_is_semi) { $1 }

context:
	| LBRACKET context_statement* RBRACKET { $2 }
	|                                      { [] }

constructor_body:
	| LBRACE semantic_body RBRACE { $2 }
	|                      KEY_UNIMPL { [] }

context_statement: 
	| assignment SEMI { $1 }
	| funcall SEMI    { $1 }

pattern_expr: 
	| pattern_expr pattern_op pattern_expr { Sexp_binary ($2, $1, $3) }
	| ELLIPSIS pattern_atomic              { Sexp_unary (Sexp_op_align_right, $2) }
	| pattern_atomic ELLIPSIS              { Sexp_unary (Sexp_op_align_left, $1) }
	| pattern_atomic                       { $1 }
	 
%inline pattern_op:
	| JOIN      { Sexp_op_join }
	| PIPE      { Sexp_op_bool_or }
	| AMPERSAND { Sexp_op_bool_and }

pattern_atomic:
	| condition { $1 }
	| LPAREN pattern_expr RPAREN { Sexp_paren $2 }

condition:
	| identifier                             { Sexp_id $1 }
	| identifier condition_op condition_expr { Sexp_binary ($2, Sexp_id $1, $3)}

condition_expr:
	| condition_expr condition_expr_op condition_expr    { Sexp_binary ($2, $1, $3) }
	| condition_expr_unary_op condition_expr %prec UNARY { Sexp_unary ($1, $2) }
	| identifier                                         { Sexp_id $1 }
	| constant                                           { Sexp_int $1 }
	| LPAREN expr = condition_expr RPAREN                { Sexp_paren expr }

semantic_body:
	statement* { $1 }
	
macro:
	KEY_MACRO identifier LPAREN arg_names RPAREN macro_body
		{ Sdef_macro { smac_id = $2; smac_args = $4; smac_body = $6 } }

macro_body:
	| LBRACE semantic_body RBRACE { $2 }

statement:
	| label          { Sstm_label $1 }
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
	| KEY_LOCAL lvalue ASSIGN expr { Sstm_assign ($2, $4) }
	| lvalue ASSIGN expr           { Sstm_assign ($1, $3) }

declaration:
	| KEY_LOCAL identifier COLON constant { Sstm_declare { sdecl_id = $2; sdecl_size = Some $4 } }
	| KEY_LOCAL identifier                { Sstm_declare { sdecl_id = $2; sdecl_size = None } }

build:
	| KEY_BUILD identifier { Sstm_build $2 }

funcall:
	| identifier LPAREN args RPAREN { Sstm_fun_call ($1, $3) }

goto:
	| KEY_GOTO jump_target { Sstm_goto $2 }

branch:
	| RES_IF expr KEY_GOTO jump_target { Sstm_branch { sbr_condition = $2; sbr_target = $4 } }

call:
	| KEY_CALL jump_target { Sstm_call $2 }
	
export:
	| KEY_EXPORT sized_export { Sstm_export $2 }
	| KEY_EXPORT varnode      { Sstm_export $2 }

return:
	| KEY_RETURN LBRACKET jump_target RBRACKET { Sstm_return $3 }

label:
	| LESS_THAN identifier GREATER_THAN { $2 }

jump_target:
	| constant LBRACKET identifier RBRACKET { Sjmp_fixed ($1, Some $3) }
	| constant                              { Sjmp_fixed ($1, None) }
	| identifier                            { Sjmp_direct $1 }
	| LBRACKET expr RBRACKET                { Sjmp_indirect $2 }
	| label                                 { Sjmp_label $1 }

lvalue:
	| bitrange                  { $1 }
	| identifier COLON constant { Sexp_sized { ssz_expr = Sexp_id $1; ssz_size = $3 } }
	| identifier                { Sexp_id $1 }
	| sized_pointer(expr)       { $1 }
	
expr:
	| expr expr_op expr         { Sexp_binary ($2, $1, $3) }
	| expr_unary_op expr_unary  { Sexp_unary ($1, $2) }
	| sized_pointer(expr_unary) { $1 }
	| expr_unary                { $1 }

expr_unary:
	| expr_funcall { $1 }
	| expr_term    { $1 }

expr_term:
	| LPAREN expr RPAREN                   { Sexp_paren $2 }
	| varnode                              { $1 }
	| bitrange                             { $1 }

expr_funcall:
	identifier LPAREN args RPAREN { Sexp_fun_call ($1, $3) }

varnode:
	| constant                  { Sexp_int $1 }
	| identifier                { Sexp_id $1 }
	| identifier COLON constant { Sexp_sized { ssz_expr = Sexp_id $1; ssz_size = $3 } }
	| constant COLON constant   { Sexp_sized { ssz_expr = Sexp_int $1; ssz_size = $3 } }

bitrange:
	identifier LBRACKET constant COMMA constant RBRACKET
		{ Sexp_bit_range { srng_id = $1; srng_start_bit = $3; srng_width = $5 }}

sized_export:
	| sized_pointer(id_expr) { $1 }

id_expr:
	| identifier { Sexp_id $1 }

%inline sized_pointer(X):
	| STAR LBRACKET identifier RBRACKET COLON constant X
		{ Sexp_sized { ssz_expr = Sexp_pointer { sptr_space = Some $3; sptr_expr = $7};  ssz_size = $6} }
	| STAR LBRACKET identifier RBRACKET X
		{ Sexp_pointer { sptr_space = Some $3; sptr_expr = $5 } }
	| STAR COLON constant X
		{ Sexp_sized { ssz_expr = Sexp_pointer { sptr_space = None; sptr_expr = $4 };  ssz_size = $3 } }
	| STAR X
		{ Sexp_pointer { sptr_space = None; sptr_expr = $2 } }

args:
	separated_list(COMMA, expr) { $1 }

arg_names:
	separated_list(COMMA, identifier) { $1 }

%inline condition_op:
	| ASSIGN        { Sexp_op_eq }
	| NOT_EQUAL     { Sexp_op_ne }
	| LESS_THAN     { Sexp_op_lt }
	| LESS_EQUAL    { Sexp_op_le }
	| GREATER_THAN  { Sexp_op_gt }
	| GREATER_EQUAL { Sexp_op_ge }

%inline condition_expr_op:
	| SPEC_OR     { Sexp_op_or }
	| SPEC_AND    { Sexp_op_and }
	| SPEC_XOR    { Sexp_op_xor }
	| SHIFT_LEFT  { Sexp_op_shift_left }
	| SHIFT_RIGHT { Sexp_op_shift_right }
	| PLUS        { Sexp_op_plus }
	| MINUS       { Sexp_op_minus }

condition_expr_unary_op:
	| MINUS { Sexp_op_negate }
	| TILDE { Sexp_op_invert }

expr_unary_op:
	| MINUS { Sexp_op_negate }
	| TILDE { Sexp_op_invert }
	| BANG  { Sexp_op_not }

%inline expr_op:
	| OR                   { Sexp_op_bool_or }
	| AND                  { Sexp_op_bool_and }
	| XOR                  { Sexp_op_bool_xor }
	| AMPERSAND            { Sexp_op_and }
	| PIPE                 { Sexp_op_or }
	| CARET                { Sexp_op_xor }
	| EQUAL                { Sexp_op_eq }
	| NOT_EQUAL            { Sexp_op_ne }
	| LESS_THAN            { Sexp_op_lt }
	| GREATER_THAN         { Sexp_op_gt }
	| LESS_EQUAL           { Sexp_op_le }
	| GREATER_EQUAL        { Sexp_op_ge }
	| SHIFT_LEFT           { Sexp_op_shift_left }
	| SHIFT_RIGHT          { Sexp_op_shift_right }
	| PLUS                 { Sexp_op_plus }
	| MINUS                { Sexp_op_minus }
	| STAR                 { Sexp_op_mul }
	| SLASH                { Sexp_op_div }
	| PERCENT              { Sexp_op_mod }
	| SIGNED_DIV           { Sexp_op_signed_div }
	| SIGNED_MOD           { Sexp_op_signed_mod }
	| SIGNED_SHIFT_LEFT    { Sexp_op_signed_shift_left }
	| SIGNED_SHIFT_RIGHT   { Sexp_op_signed_shift_right }
	| SIGNED_GREATER_EQUAL { Sexp_op_signed_ge }
	| SIGNED_LESS_EQUAL    { Sexp_op_signed_le }
	| SIGNED_GREATER_THAN  { Sexp_op_signed_gt }
	| SIGNED_LESS_THAN     { Sexp_op_signed_lt }
	| FLOAT_EQUAL          { Sexp_op_float_eq }
	| FLOAT_NOT_EQUAL      { Sexp_op_float_ne }
	| FLOAT_DIV            { Sexp_op_float_div }
	| FLOAT_MUL            { Sexp_op_float_mul }
	| FLOAT_MINUS          { Sexp_op_float_minus }
	| FLOAT_PLUS           { Sexp_op_float_plus }
    | FLOAT_GREATER_EQUAL  { Sexp_op_float_ge }
	| FLOAT_GREATER_THAN   { Sexp_op_float_gt }
    | FLOAT_LESS_EQUAL     { Sexp_op_float_le }
	| FLOAT_LESS_THAN      { Sexp_op_float_lt }

identifiers:
	identifier+ { $1 }

token_field:
	identifier ASSIGN LPAREN constant COMMA constant RPAREN token_field_mod*
		{ { stf_id = $1; stf_start_bit = $4; stf_end_bit = $6; stf_mods = $8 } }

token_field_mod:
	| signed { Stfm_signed $1 }
	| hex    { Stfm_hex $1 }

space_is_default:
	| KEY_DEFAULT { true }

identifier:
	located(ID) { $1 }

text:
	located(TEXT) { $1 }

%inline located(X):
	x=X { Position.with_poss $startpos $endpos x }

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

semi_is_join:
	| { semi_is_join := true }

semi_is_semi:
	| { semi_is_join := false }

