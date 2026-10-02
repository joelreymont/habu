\ Whole-record stores into both fixed storage definers survive image restore.
require lib/test.f
require test/checker-assert.f

package QUOT-PERSIST-TEST

STRUCTURE entry 0 DERIVE addr
   FIELD handler [ n -- n ]
;STRUCTURE

STRUCTURE generic-entry 1 DERIVE addr
   FIELD value a
   FIELD handler [ a -- a ]
;STRUCTURE

STRUCTURE hook 0 FIELD handler [ n -- n ] ;STRUCTURE
SUMTYPE choice 0
   VARIANT empty ;VARIANT
   VARIANT scalar n ;VARIANT
   VARIANT present hook ;VARIANT
;SUMTYPE
STRUCTURE holder 0 FIELD choice choice ;STRUCTURE
STRUCTURE envelope 0 FIELD item holder ;STRUCTURE

TYPED-VARIABLE ROW entry
1 LAYOUT-BUFFER LB entry
TYPED-VARIABLE GENERIC-ROW generic-entry<n>
TYPED-VARIABLE PRESENT-ROW envelope
TYPED-VARIABLE SCALAR-ROW envelope
TYPED-VARIABLE EMPTY-ROW envelope
3 LAYOUT-BUFFER BATCH envelope
variable LOOKALIKE

: READ-CHOICE ( envelope -- n )
   ENVELOPE-UNMAKE HOLDER-UNMAKE
   MATCH choice
      empty OF 0 ENDOF
      scalar OF 10 + ENDOF
      present OF HOOK-UNMAKE 35 swap execute ENDOF
   ;MATCH ;

: INIT ( -- )
   [: 7 + ;] ENTRY-MAKE ROW !
   [: 8 + ;] ENTRY-MAKE 0 LB !
   10 [: 7 + ;] GENERIC-ENTRY-MAKE GENERIC-ROW !
   [: 7 + ;] HOOK-MAKE construct choice present HOLDER-MAKE ENVELOPE-MAKE PRESENT-ROW !
   cp@ 8 - LOOKALIKE !
   LOOKALIKE @ construct choice scalar HOLDER-MAKE ENVELOPE-MAKE SCALAR-ROW !
   construct choice empty HOLDER-MAKE ENVELOPE-MAKE EMPTY-ROW !
   [: 6 + ;] HOOK-MAKE construct choice present HOLDER-MAKE ENVELOPE-MAKE 0 BATCH !
   LOOKALIKE @ construct choice scalar HOLDER-MAKE ENVELOPE-MAKE 1 BATCH !
   construct choice empty HOLDER-MAKE ENVELOPE-MAKE 2 BATCH ! ;

: CHECK ( -- )
   T-RESET
   35 ROW ENTRY-HANDLER @ execute 42 T=
   34 0 LB ENTRY-HANDLER @ execute 42 T=
   35 GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute 42 T=
   PRESENT-ROW @ READ-CHOICE 42 T=
   SCALAR-ROW @ READ-CHOICE LOOKALIKE @ 10 + T=
   EMPTY-ROW @ READ-CHOICE 0 T=
   0 BATCH @ READ-CHOICE 41 T=
   1 BATCH @ READ-CHOICE LOOKALIKE @ 10 + T=
   2 BATCH @ READ-CHOICE 0 T=
   s" BAD-GENERIC ( bool -- n ) GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;

INIT CHECK
;package
