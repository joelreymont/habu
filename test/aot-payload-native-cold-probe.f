\ The verifier dependency is loaded exactly as check-verify-child.f loads it.
\ This fresh process witnesses its symbol state; the real child tests completion.
tick-order@
package PAYLOAD-NATIVE-COLD-PROBE
variable ENTRY-ORDER
PTR-VARIABLE ENTRY-OWNER
ENTRY-OWNER !
ENTRY-ORDER !
;package

require src/habu/verify-source.f

package PAYLOAD-NATIVE-COLD-PROBE

ENTRY-ORDER @ ENTRY-OWNER @ VERIFY:ENTRY-TICK-ORDER!

\ Read the verifier's existing declaration owner without importing a seed row.
: OWNER-XT ( n -- n )
   {: off:n :}
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   off CELL + CHECKER-OWNER-GUARD:VALIDATE
   off + CELL-VIEW @ dup 0= if E-NCOMP-OWNER throw then ;

CAST: SYM-ACTION ( n -- [ ptr u8 n -- n ] )

: RECORD-SYM? ( ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-VERIFY-RECORD-SYM-OFF OWNER-XT SYM-ACTION execute ;

: FIND-SYM ( ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-VERIFY-FIND-SYM-OFF OWNER-XT SYM-ACTION execute ;

public

: RUN ( -- )
   s" PAYLOAD-NATIVE:BUMP" RECORD-SYM? 0<> if 79 throw then
   s" BUMP" FIND-SYM 0<> if 79 throw then
   s" native graph cold verifier: ok" type cr ;

;package

using PAYLOAD-NATIVE
PAYLOAD-NATIVE-COLD-PROBE:RUN
;using
