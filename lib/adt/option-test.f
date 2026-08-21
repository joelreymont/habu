\ option-test.f - unified OPTION declaration contract.

require lib/test.f
require test/checker-assert.f
require lib/adt/option.f

T-RESET

package OPTION-TEST

using REFLECT
using TFAM

private

: YES ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! -1 T= ;
: NO ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! 0 T= ;
: UNRES ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! 1 T= ;

: FAM$ ( -- ptr u8 n ptr u8 n ) s" option" s" OPTION" ;

: PUB ( -- n )
   s" OPTION" XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? dup TTRUE
   if XREF-START else drop 0 then ;

variable NONE#
variable SOME#

: NOTE ( ptr a -- )
   XREF-NAME$
   2dup s" NONE" XREF-STR=CI if 2drop 1 NONE# +! exit then
   2dup s" SOME" XREF-STR=CI if 2drop 1 SOME# +! exit then
   2drop 0 0= 0= TTRUE ;

s" OPTION records the general sum kind" T-LABEL
FAM$ KIND TK-SUM T=
s" OPTION does not record the compact enum kind" T-LABEL
FAM$ KIND TK-ENUM = TFALSE

: SURFACE ( -- )
   PUB {: wid:n :}
   0 NONE# !
   0 SOME# !
   ndict@ 0 ?do
      i XREF-REC dup XREF-WORDLIST wid = if
         NOTE
      else
         drop
      then
   loop
   NONE# @ 1 T=
   SOME# @ 1 T= ;

: META ( -- )
   FAM$ FAMS 1 T=
   FAM$ ARITY 1 T=
   FAM$ WIDTH 2 T=
   FAM$ VIS 1 T=
   FAM$ VARS 2 T=
   FAM$ 0 ARM$ s" none" T$=
   FAM$ 1 ARM$ s" some" T$=
   FAM$ 2 ARM$ s" <missing>" T$=
   FAM$ 0 ARM-CTOR$ s" OPTION" T$=
   FAM$ 1 ARM-CTOR$ s" OPTION" T$=
   FAM$ 0 ARM-FLDS 0 T=
   FAM$ 1 ARM-FLDS 1 T=
   FAM$ 1 s" value" ARM-SLOT 0 T=
   FAM$ 1 s" value" ARM-CELLS 1 T=
   FAM$ 1 s" payload" ARM-SLOT -1 T= ;

: EFFECTS ( -- )
   s" OPT-NONE-N ( -- option<n> ) OPTION:NONE" YES
   s" OPT-NONE-R ( -- option<r> ) OPTION:NONE" YES
   s" OPT-SOME-N ( n -- option<n> ) OPTION:SOME" YES
   s" OPT-SOME-I ( idx -- option<idx> ) OPTION:SOME" YES
   s" OPT-X-NAME ( -- option<n> ) OPTION:NOTHING" UNRES
   s" OPT-X-NONE ( n -- option<n> ) OPTION:NONE" NO
   s" OPT-X-SOME ( -- option<n> ) OPTION:SOME" NO
   s" OPT-X-ROLE ( len -- option<idx> ) OPTION:SOME" NO
   s" OPT-X-INST ( idx -- option<len> ) OPTION:SOME" NO ;

: REBUILD ( option<n> -- option<n> )
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF {: v:n :} v OPTION:SOME ENDOF
   ;MATCH ;

: TAG ( option<n> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF drop 1 ENDOF
   ;MATCH ;

: VALUE ( option<n> -- n )
   MATCH option
      none OF -1 ENDOF
      some OF {: v:n :} v ENDOF
   ;MATCH ;

: ROUND ( -- )
   OPTION:NONE REBUILD TAG 0 T=
   42 OPTION:SOME REBUILD TAG 1 T=
   OPTION:NONE REBUILD VALUE -1 T=
   42 OPTION:SOME REBUILD VALUE 42 T= ;

public

: RUN ( -- )
   s" exact public constructor wordlist" T-LABEL SURFACE
   s" registry shape and declaration-order tags" T-LABEL META
   s" constructor effects and nominal roles" T-LABEL EFFECTS
   s" compiled MATCH and constructor round trip" T-LABEL ROUND
   T-REPORT ;

;using
;using

;package

OPTION-TEST:RUN

