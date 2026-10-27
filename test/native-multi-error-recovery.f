\ A counted native checker refusal ends its pending definition without
\ publishing it, and the surrounding source keeps its earlier definitions.
1 set-tier
require src/habu/layout.f

: SHADOW-RECOVERY ( n -- n ) 1+ ;
: TOP-RECOVERY ( n -- n ) 1+ ;
: EXPORT-RECOVERY ( n -- n ) 1+ ;

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

\ A trusted body is not judged, but its walk binds a recovery row all the same,
\ so it is withheld as a checked body is and the load reads on.
: CHECK-TRUSTED ( -- )
   FINISH 1 <> if s" native trusted recovery count" 1 die then
   s" TRUSTED-FOLLOW" get-current search-wl 0<> if s" native trusted dependent published" 1 die then ;

START
: TRUSTED-BAD ( n -- n ) drop ;
TRUSTED: TRUSTED-FOLLOW ( n -- n ) TRUSTED-BAD ;
CHECK-TRUSTED

\ A definer whose head binds a recovery row is withheld, though its does>
\ clause, checked after the head, binds none.
: CHECK-DEFINER ( -- )
   FINISH 1 <> if s" native definer recovery count" 1 die then
   s" DEFINER-FOLLOW" get-current search-wl 0<> if s" native definer dependent published" 1 die then ;

START
: DEFINER-BAD ( n -- n ) drop ;
: DEFINER-FOLLOW ( n -- ) DEFINER-BAD drop create 0 , does> ( -- n ) @ ;
CHECK-DEFINER

\ A top-level step runs at once, so it is typed against the global word the
\ engine runs, not the refused definition's recovery row: `c@` on its cell is
\ refused rather than admitted and run.
: CHECK-TOP ( -- )
   [: s" 41 TOP-RECOVERY c@ drop" evaluate-closed ;] catch
   70 <> if s" native top step bound a recovery row" 1 die then
   FINISH 1 <> if s" native top recovery count" 1 die then
   s" TOP-RECOVERY" get-current search-wl 0<> if s" native top refusal published" 1 die then ;

START
: TOP-RECOVERY ( n -- ptr u8 ) drop ;
CHECK-TOP

\ EXPORT runs at once too, so it publishes the global it finds under the
\ refused private definition's name with that global's effect, and a top-level
\ step using the alias as the refused row's type is refused. The check runs in
\ the private section, where the refusal would have been published.
: CHECK-EXPORT ( -- )
   [: s" 41 EXPORT-RECOVERY c@ drop" evaluate-closed ;] catch
   70 <> if s" native export bound a recovery row" 1 die then
   FINISH 1 <> if s" native export recovery count" 1 die then
   s" EXPORT-RECOVERY" get-current search-wl 0<> if s" native export refusal published" 1 die then ;

START
private
: EXPORT-RECOVERY ( n -- ptr u8 ) drop ;
public
EXPORT EXPORT-RECOVERY
private
CHECK-EXPORT
public

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
