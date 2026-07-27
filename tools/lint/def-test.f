\ def-test.f - focused structural defining-role classifier fixtures.

require lib/test.f
require tools/lint/def.f

package LINT-DEF-TEST
private

57 constant FORM#

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
      37 of s" BUFFER-E" endof
      38 of s" CODEGEN:BUFFER" endof
      39 of s" CODEGEN:BUFFER-E" endof
      40 of s" TASK" endof
      41 of s" +USER" endof
      42 of s" FACILITY" endof
      43 of s" TASK:TASK" endof
      44 of s" TASK:+USER" endof
      45 of s" TASK:FACILITY" endof
      46 of s" TR-FILES:" endof
      47 of s" GE-FILES:" endof
      48 of s" IOP:" endof
      49 of s" CONST" endof
      50 of s" ARR" endof
      51 of s" EXTENT:" endof
      52 of s" FREE-EXTENT:" endof
      53 of s" EXTPROD:" endof
      54 of s" TENSOR:" endof
      55 of s" ITENSOR:" endof
      56 of s" SPEC:" endof
      E-TBL-BOUNDS throw
   endcase ;

: FORM-KIND ( n -- n ) {: k:n :}
   k 7 < if LINT-DEF:COLON exit then
   k 13 < if k 5 - exit then
   LINT-DEF:DATA ;

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

: DIRECT$ ( n -- ptr u8 n ) {: k:n :}
   SB-RESET
   k FORM$ SB-APPEND
   32 SB-APPEND-C s" NAME" SB-APPEND
   k FORM-KIND dup LINT-DEF:DATA <> if
      32 SB-APPEND-C s" ( -- )" SB-APPEND 32 SB-APPEND-C
      CLOSE$ SB-APPEND
   else
      drop
   then
   SB$ ;

: SOME= ( option<n> n -- ) {: want:n :}
   MATCH option
      none OF -1 want T= ENDOF
      some OF want T= ENDOF
   ;MATCH ;

: NONE? ( option<n> -- )
   MATCH option
      none OF LINT-TRUE TTRUE ENDOF
      some OF drop LINT-FALSE TTRUE ENDOF
   ;MATCH ;

: DIRECT-CASES ( -- )
   LINT-DEF:FORM-COUNT FORM# T=
   FORM# 0 ?do
      i DIRECT$ LINT-LEX:SOURCE
      0 LINT-DEF:DIRECT-KIND i FORM-KIND T=
      0 LINT-DEF:NAME-I 1 SOME=
      0 LINT-DEF:EXPORT? TFALSE
      0 LINT-DEF:EXPORT-I NONE?
      i FORM-KIND LINT-DEF:DATA <> if
         LINT-LEX:COUNT 1- i FORM-KIND LINT-DEF:CLOSE? TTRUE
      then
   loop ;

: MIXED-AND-LEGACY ( -- )
   s" cHeCkEd: Name ;" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:COLON T=
   s" newtype T 0" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:DATA T=
   s" enum E ;enum" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:ENUM T=
   2 LINT-DEF:ENUM LINT-DEF:CLOSE? TTRUE
   s" structure S ;structure" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:STRUCTURE T=
   2 LINT-DEF:STRUCTURE LINT-DEF:CLOSE? TTRUE
   s" sumtype S ;sumtype" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:SUMTYPE T=
   s" product P ;product" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:PRODUCT T=
   s" deftype T" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:DATA T= ;

: EXPORT-CASES ( -- )
   s" EXPORT GLOBAL:COUNT>N" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:NONE T=
   0 LINT-DEF:EXPORT? TTRUE
   0 LINT-DEF:EXPORT-I 1 SOME=
   1 LINT-DEF:EXPORT? TFALSE
   s" EXPORT" LINT-LEX:SOURCE
   0 LINT-DEF:EXPORT-I NONE?
   s" EXPORT-X NAME" LINT-LEX:SOURCE
   0 LINT-DEF:EXPORT? TFALSE
   0 LINT-DEF:EXPORT-I NONE? ;

: ALL-INERT ( -- )
   0 begin dup LINT-LEX:COUNT < while
      dup LINT-DEF:DIRECT-KIND LINT-DEF:NONE T=
      dup LINT-DEF:EXPORT? TFALSE
      1+
   repeat drop ;

: STRING$ ( -- ptr u8 n )
   SB-RESET
   s" s" SB-APPEND 34 SB-APPEND-C
   s"  : HIDDEN EXPORT HIDDEN" SB-APPEND
   34 SB-APPEND-C
   SB$ ;

: OPAQUE-AND-MISSING ( -- )
   s" ( : HIDDEN EXPORT HIDDEN )" LINT-LEX:SOURCE ALL-INERT
   STRING$ LINT-LEX:SOURCE ALL-INERT
   s" PRIM: HIDDEN PE-N PE-IN PRIM;" LINT-LEX:SOURCE ALL-INERT
   s" :" LINT-LEX:SOURCE
   0 LINT-DEF:NAME-I NONE?
   s" : (COMMENT-NAME) ;" LINT-LEX:SOURCE
   0 LINT-DEF:NAME-I NONE?
   s" : NEAR ;" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:COLON T=
   2 LINT-DEF:SUMTYPE LINT-DEF:CLOSE? TFALSE
   2 LINT-DEF:NONE LINT-DEF:CLOSE? TFALSE
   2 LINT-DEF:DATA LINT-DEF:CLOSE? TFALSE
   s" COLON: NAME" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:NONE T=
   s" NEWTYPE-X NAME" LINT-LEX:SOURCE
   0 LINT-DEF:DIRECT-KIND LINT-DEF:NONE T= ;

public

: RUN ( -- )
   T-RESET
   DIRECT-CASES
   MIXED-AND-LEGACY
   EXPORT-CASES
   OPAQUE-AND-MISSING
   T-REPORT ;

RUN

;package
