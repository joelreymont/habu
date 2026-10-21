\ A counted native checker refusal ends its pending definition without
\ publishing it, and the surrounding source keeps its earlier definitions.
1 set-tier
require src/habu/layout.f

: SHADOW-RECOVERY ( n -- n ) 1+ ;

package NATIVE-MULTI-RECOVERY
public

: BEFORE ( -- n ) 17 ;
: START ( -- ) MULTI-ERR-BEGIN ;
: FINISH ( -- n ) MULTI-ERR-END ;
: CHECK-RECOVERY ( -- )
   FINISH 1 <> if s" native multi-error count" 1 die then
   data-base PEND-CELL + @ 0<> if s" native pending definition" 1 die then
   s" BAD" get-current search-wl 0<> if s" native refused definition published" 1 die then
   s" FOLLOW" get-current search-wl 0<> if s" native recovery dependent published" 1 die then
   s" FOLLOW-2" get-current search-wl 0<> if s" native recovery chain published" 1 die then
   s" QUALIFIED" get-current search-wl 0<> if s" native qualified recovery published" 1 die then ;

START
: BAD ( n -- n ) drop ;
: FOLLOW ( n -- n ) BAD ;
: FOLLOW-2 ( n -- n ) FOLLOW ;
: QUALIFIED ( n -- n ) NATIVE-MULTI-RECOVERY:BAD ;
CHECK-RECOVERY

: CHECK-STALE ( -- )
   FINISH 1 <> if s" native stale recovery row" 1 die then
   s" STALE" get-current search-wl 0<> if s" native stale recovery published" 1 die then ;

START
: STALE ( n -- n ) BAD ;
CHECK-STALE

: AFTER ( -- n ) 25 ;
: CHECK-AFTER ( -- )
   BEFORE AFTER + 42 <> if s" native multi-error continuation" 1 die then ;
CHECK-AFTER

: CHECK-SHADOW ( -- )
   FINISH 1 <> if s" native shadow recovery count" 1 die then
   s" SHADOW-RECOVERY" get-current search-wl 0<> if s" native shadow refusal published" 1 die then
   s" SHADOW-FOLLOW" get-current search-wl 0<> if s" native shadow dependent published" 1 die then ;

START
: SHADOW-RECOVERY ( n -- n ) drop ;
: SHADOW-GOOD ( -- n ) 17 ;
: SHADOW-FOLLOW ( n -- n ) SHADOW-RECOVERY ;
CHECK-SHADOW

;package

: GLOBAL-START ( -- ) MULTI-ERR-BEGIN ;
: GLOBAL-FINISH ( -- n ) MULTI-ERR-END ;
GLOBAL-START
: GLOBAL-RECOVERY ( n -- n ) drop ;
: GLOBAL-GOOD ( -- n ) 17 ;

package NATIVE-RECOVERY-QUAL
public
: QUAL-FOLLOW ( n -- n ) NATIVE-RECOVERY-QUAL:GLOBAL-RECOVERY ;
: CHECK-QUAL ( -- )
   GLOBAL-FINISH 1 <> if s" native qualified global recovery count" 1 die then
   s" QUAL-FOLLOW" get-current search-wl 0<> if s" native qualified global dependent published" 1 die then ;
CHECK-QUAL
;package

GLOBAL-START
: GLOBAL-RECOVERY-CLOSED ( n -- n ) drop ;
: GLOBAL-GOOD-CLOSED ( -- n ) 18 ;
package NATIVE-RECOVERY-OTHER
public
: CLOSED-FOLLOW ( n -- n ) NATIVE-RECOVERY-QUAL:GLOBAL-RECOVERY-CLOSED ;
: CHECK-CLOSED ( -- )
   GLOBAL-FINISH 2 <> if s" native closed qualified recovery count" 1 die then
   s" CLOSED-FOLLOW" get-current search-wl 0<> if s" native closed qualified dependent published" 1 die then ;
CHECK-CLOSED
;package

\ A refused public definition must not hide a live private word of the same
\ name while the recovery run continues, or after its recovery rows expire.
package NATIVE-RECOVERY-PENDING
private
: ACTUAL ( n -- n ) 1+ ;
public
: START ( -- ) MULTI-ERR-BEGIN ;
: FINISH ( -- n ) MULTI-ERR-END ;
: CHECK-COUNT ( -- )
   FINISH 2 <> if s" native pending fallback count" 1 die then ;
START
: OTHER-BAD ( n -- n ) drop ;
: ACTUAL ( n -- n ) drop ;
: USE-ACTUAL ( n -- n ) ACTUAL ;
CHECK-COUNT
: CHECK-ACTUAL ( -- )
   41 USE-ACTUAL 42 <> if s" native pending private fallback" 1 die then ;
CHECK-ACTUAL
;package

package NATIVE-RECOVERY-EXPIRED
private
: ACTUAL ( n -- n ) 1+ ;
public
: START ( -- ) MULTI-ERR-BEGIN ;
: FINISH ( -- n ) MULTI-ERR-END ;
: CHECK-COUNT ( -- )
   FINISH 1 <> if s" native expired fallback count" 1 die then ;
START
: ACTUAL ( n -- n ) drop ;
CHECK-COUNT
: USE-EXPIRED ( n -- n ) ACTUAL ;
: CHECK-EXPIRED ( -- )
   41 USE-EXPIRED 42 <> if s" native expired private fallback" 1 die then ;
CHECK-EXPIRED
;package

s" ok" type cr
