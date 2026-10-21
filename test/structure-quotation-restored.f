\ Loaded by a fresh saved image after its route definitions were captured.
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
