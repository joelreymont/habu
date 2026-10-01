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

TYPED-VARIABLE ROW entry
1 LAYOUT-BUFFER LB entry
TYPED-VARIABLE GENERIC-ROW generic-entry<n>

: INIT ( -- )
   [: 7 + ;] ENTRY-MAKE ROW !
   [: 8 + ;] ENTRY-MAKE 0 LB !
   10 [: 7 + ;] GENERIC-ENTRY-MAKE GENERIC-ROW ! ;

: CHECK ( -- )
   T-RESET
   35 ROW ENTRY-HANDLER @ execute 42 T=
   34 0 LB ENTRY-HANDLER @ execute 42 T=
   35 GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute 42 T=
   s" BAD-GENERIC ( bool -- n ) GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;

INIT CHECK
;package
