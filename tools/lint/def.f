\ def.f - one audited inventory of words that define dictionary names.
\
\ Native forms come from the engine dictionary and docs/typed-top-level.md.
\ Type/storage forms cover the checker's top-level declarations. Project forms
\ cover every executable `create` and definition-generating `evaluate` owner.
\ Registry grammars that publish rows rather than dictionary words are absent.

require lib/adt/option.f
require lib/errors.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package LINT-DEF
public

0 constant NONE
1 constant COLON
2 constant SUMTYPE
3 constant PRODUCT
4 constant ENUM
5 constant STRUCTURE
6 constant VALUE-RECORD
7 constant LOW-STRUCTURE
8 constant DATA

private

55 constant FORM#

: WORD? ( n -- bool )
   LINT-LEX:KIND@ LINT-LEX:WORD = ;

: TOK=CI ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= if LINT-FALSE exit then
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

: FORM-KIND ( n -- n ) {: k:n :}
   k 7 < if COLON exit then
   k 13 < if k 5 - exit then
   DATA ;

: FORM$ ( n -- ptr u8 n )
   case
      0 of s" :" endof
      1 of s" +:" endof
      2 of s" CHECKED:" endof
      3 of s" TRUSTED:" endof
      4 of s" KERNEL:" endof
      5 of s" CAST:" endof
      6 of s" MODEL:" endof
      7 of s" SUMTYPE" endof
      8 of s" PRODUCT" endof
      9 of s" ENUM" endof
      10 of s" STRUCTURE" endof
      11 of s" VALUE-RECORD" endof
      12 of s" BEGIN-STRUCTURE" endof
      13 of s" constant" endof
      14 of s" 2constant" endof
      15 of s" fconstant" endof
      16 of s" variable" endof
      17 of s" 2variable" endof
      18 of s" fvariable" endof
      19 of s" create" endof
      20 of s" value" endof
      21 of s" defer" endof
      22 of s" LAYOUT-BUFFER" endof
      23 of s" DEFER-LAYOUT-BUFFER" endof
      24 of s" TYPED-BUFFER" endof
      25 of s" TYPED-VARIABLE" endof
      26 of s" PTR-VARIABLE" endof
      27 of s" PTR-FIELD:" endof
      28 of s" CFIELD:" endof
      29 of s" +FIELD" endof
      30 of s" NEWTYPE" endof
      31 of s" DEFTYPE" endof
      32 of s" DEFLINEAR" endof
      33 of s" ENUM+" endof
      34 of s" ENUM4+" endof
      35 of s" BUFFER:" endof
      36 of s" BUFFER" endof
      37 of s" CODEGEN:BUFFER" endof
      38 of s" TASK" endof
      39 of s" +USER" endof
      40 of s" FACILITY" endof
      41 of s" TASK:TASK" endof
      42 of s" TASK:+USER" endof
      43 of s" TASK:FACILITY" endof
      44 of s" TR-FILES:" endof
      45 of s" GE-FILES:" endof
      46 of s" IOP:" endof
      47 of s" CONST" endof
      48 of s" ARR" endof
      49 of s" EXTENT:" endof
      50 of s" FREE-EXTENT:" endof
      51 of s" EXTPROD:" endof
      52 of s" TENSOR:" endof
      53 of s" ITENSOR:" endof
      54 of s" SPEC:" endof
      E-TBL-BOUNDS throw
   endcase ;

public

: FORM-COUNT ( -- n )
   FORM# ;

: DIRECT-KIND ( n -- n ) {: tok:n :}
   0 begin dup FORM# < while
      tok over FORM$ TOK=CI if
         FORM-KIND exit
      then
      1+
   repeat drop NONE ;

private

: CLOSE$ ( n -- ptr u8 n )
   case
      LINT-DEF:COLON of s" ;" endof
      LINT-DEF:SUMTYPE of s" ;SUMTYPE" endof
      LINT-DEF:PRODUCT of s" ;PRODUCT" endof
      LINT-DEF:ENUM of s" ;ENUM" endof
      LINT-DEF:STRUCTURE of s" ;STRUCTURE" endof
      LINT-DEF:VALUE-RECORD of s" END-VALUE-RECORD" endof
      LINT-DEF:LOW-STRUCTURE of s" END-STRUCTURE" endof
      E-TBL-BOUNDS throw
   endcase ;

public

: CLOSE? ( n n -- bool ) {: tok:n kind:n :}
   kind NONE = kind DATA = or if LINT-FALSE exit then
   tok kind CLOSE$ TOK=CI ;

: NAME-I ( n -- option<n> ) {: tok:n :}
   tok DIRECT-KIND NONE = if OPTION:NONE exit then
   tok 1+ dup LINT-LEX:COUNT >= if drop OPTION:NONE exit then
   dup LINT-LEX:KIND@ LINT-LEX:WORD = if
      OPTION:SOME
   else
      drop OPTION:NONE
   then ;

: EXPORT? ( n -- bool )
   s" EXPORT" TOK=CI ;

: EXPORT-I ( n -- option<n> ) {: tok:n :}
   tok EXPORT? 0= if OPTION:NONE exit then
   tok 1+ dup LINT-LEX:COUNT >= if drop OPTION:NONE exit then
   dup LINT-LEX:KIND@ LINT-LEX:WORD = if OPTION:SOME else drop OPTION:NONE then ;

;package
