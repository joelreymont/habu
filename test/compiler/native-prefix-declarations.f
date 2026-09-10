\ Standalone process: retain the compiler while replacing its dictionary prefix.
: PF-OLD-SIGNATURE ( -- bool ) 0 0= ;

package PREFIX-DECLARATIONS-TEST
private

TRUSTED: RESET ( -- )
   0 set-check
   0 set-top-check
   CHECKER-RESET-SOURCE
   IMK-NDICT0 @ 1 - seed-ndict! ;

: LOAD-CORE ( -- )
   s" src/core/util.f" included
   s" src/core/cell.f" included ;

TRUSTED: REPLAY ( -- n )
   s" variable V 40 constant BASE create ROW 2 cells allot : PF-OLD-SIGNATURE ( -- n ) 2 ; : PF-STORE ( -- ) BASE V ! PF-OLD-SIGNATURE V +! V @ ROW ! ; : PF-READ ( -- n ) ROW @ ; PF-STORE PF-READ" evaluate ;

: RUN ( -- )
   s" CHECKER-RESET-SOURCE" 0 search-wl 0<> if
      s" checker reset is visible outside the engine" 76 die
   then
   s" PF-BAD-RESET ( -- ) CHECKER-RESET-SOURCE" CHECK-CANDIDATE! 0<> if
      s" ordinary code can reset checker state" 76 die
   then
   RESET
   LOAD-CORE
   REPLAY 42 <> if s" native prefix declarations failed" 76 die then ;

' RUN
;package
execute
