\ Loaded by a fresh saved image after its route and stored-record definitions
\ were captured. Its first-tier checks compile at tier 0 off x86-64 Linux, so
\ this session never saves; switching the records and saving runs separately.
require src/compiler/native/compiler.f
1 set-tier

package QUOT-FIELD-TEST

: FIRST-TIER ( -- n ) HB-TARGET-LINUX-X86-64? if 1 else 0 then ;
FIRST-TIER set-tier
: CHECK-SAVED-FIRST ( -- )
   SAVED-BOX @ ROUTE-BOX-UNMAKE ROUTE-UNMAKE
   {: method:route-text pattern:route-text handler :}
   method ROUTE-TEXT-UNMAKE s" GET" T$=
   pattern ROUTE-TEXT-UNMAKE s" /item" T$=
   7 REQUEST-MAKE 13 RESPONSE-MAKE handler execute
   SEEN @ 77 T= ;
1 set-tier

: CHECK-SAVED-T1 ( -- )
   SAVED-BOX @ ROUTE-BOX-UNMAKE ROUTE-UNMAKE
   {: method:route-text pattern:route-text handler :}
   method ROUTE-TEXT-UNMAKE s" GET" T$=
   pattern ROUTE-TEXT-UNMAKE s" /item" T$=
   7 REQUEST-MAKE 13 RESPONSE-MAKE handler execute
   SEEN @ 77 T= ;

: FRESH-CHECK ( -- )
   T-RESET
   CHECK-SAVED-FIRST
   CHECK-SAVED-T1
   129 0 ?do i ADD loop
   0 DISPATCH SEEN @ 20 T=
   128 DISPATCH SEEN @ 1020 T=
   ROUTES-RELEASE
   T-REPORT ;

FRESH-CHECK
RUN
;package

package QUOT-PERSIST-TEST

: PERSIST-FIRST-TIER ( -- n ) HB-TARGET-LINUX-X86-64? if 1 else 0 then ;
PERSIST-FIRST-TIER set-tier
: SAVED-FIRST ( -- )
   35 GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute 42 T=
   PRESENT-ROW @ READ-CHOICE 42 T=
   SCALAR-ROW @ READ-CHOICE LOOKALIKE @ 10 + T=
   EMPTY-ROW @ READ-CHOICE 0 T=
   0 BATCH @ READ-CHOICE 41 T=
   1 BATCH @ READ-CHOICE LOOKALIKE @ 10 + T=
   2 BATCH @ READ-CHOICE 0 T= ;
1 set-tier

: SAVED-T1 ( -- )
   35 GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute 42 T=
   PRESENT-ROW @ READ-CHOICE 42 T=
   SCALAR-ROW @ READ-CHOICE LOOKALIKE @ 10 + T=
   EMPTY-ROW @ READ-CHOICE 0 T=
   0 BATCH @ READ-CHOICE 41 T=
   1 BATCH @ READ-CHOICE LOOKALIKE @ 10 + T=
   2 BATCH @ READ-CHOICE 0 T= ;

CHECK
T-RESET SAVED-FIRST SAVED-T1 T-REPORT
;package
