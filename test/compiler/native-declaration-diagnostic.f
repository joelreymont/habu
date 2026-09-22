\ Diagnostic recovery preserves the transaction error and the caller's stack.
\ Tier-neutral by design: the transaction is driven through its own API and the
\ recovery asserted is the transaction's, not a compiled body's.
require lib/test.f
require src/core/declaration-transaction.f

package DECLARATION-DIAGNOSTIC-TEST

create STATE DECLARATION-TRANSACTION:STATE-CELLS cells allot
create ROW DECLARATION-TRANSACTION:ROW-CELLS cells allot
variable CALLS
variable SEEN-PRIMARY
variable SEEN-CLEANUP
variable DIAGNOSTIC-THROWS

-7192 constant E-BODY
-7191 constant E-CLEANUP
-7193 constant E-DIAGNOSTIC

: DIAGNOSTIC ( n n -- ) {: primary:n cleanup:n :}
   1 CALLS +!
   primary SEEN-PRIMARY !
   cleanup SEEN-CLEANUP !
   DIAGNOSTIC-THROWS @ if E-DIAGNOSTIC throw then ;

: KEEP ( n -- n ) ;
: ROLLBACK ( n -- n ) E-CLEANUP throw ;
: RELEASE ( -- ) ;
: BODY ( -- ) E-BODY throw ;

: INIT ( bool -- )
   DIAGNOSTIC-THROWS !
   0 CALLS ! 0 SEEN-PRIMARY ! 0 SEEN-CLEANUP !
   STATE ROW 1 [: DIAGNOSTIC ;] DECLARATION-TRANSACTION:INIT
   STATE 1 1 [: KEEP ;] [: KEEP ;] [: KEEP ;] [: ROLLBACK ;] [: RELEASE ;]
   DECLARATION-TRANSACTION:REGISTER
   STATE DECLARATION-TRANSACTION:SEAL ;

: TRANSACT ( -- ) STATE [: BODY ;] DECLARATION-TRANSACTION:RUN ;

: CHECK-FAILURE ( bool -- )
   INIT
   314159 [: TRANSACT ;] catch E-BODY T= 314159 T=
   CALLS @ 1 T=
   SEEN-PRIMARY @ E-BODY T=
   SEEN-CLEANUP @ E-CLEANUP T=
   STATE DECLARATION-TRANSACTION:DEPTH 0 T=
   STATE DECLARATION-TRANSACTION:POISONED? TTRUE ;

public
: RUN ( -- )
   T-RESET
   false CHECK-FAILURE
   true CHECK-FAILURE
   T-REPORT ;
;package

DECLARATION-DIAGNOSTIC-TEST:RUN
