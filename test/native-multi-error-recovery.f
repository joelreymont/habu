\ A counted native checker refusal ends its pending definition without
\ publishing it, and the surrounding source keeps its earlier definitions.
1 set-tier
require src/habu/layout.f

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

;package
s" ok" type cr
