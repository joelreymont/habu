\ Spilled DATA addresses must stay intact through their four move-wide lanes.
\ Tier 1 first: an address class spills only in the optimizing compiler, so
\ the four move-wide lanes this loop forces exist at tier 1 alone.
1 set-tier

require lib/test.f

package ADDRESS-SPILL-TEST
private

create DATA 24 cells allot

: AT ( ptr n n -- n )
   {: p:ptr index:n :} p index cells + @ ;


\ Two DATA addresses in the conditional loop force an address class to spill.
: COUNT ( n -- n )
   {: k:n :}
   1 k 0 ?do DATA i AT DATA k AT = if 1+ then loop ;


: RUN ( -- )
   T-RESET
   4 DATA ! 7 DATA cell+ ! 4 DATA 2 cells + !
   9 DATA 3 cells + ! 4 DATA 4 cells + !
   0 COUNT 1 T=
   1 COUNT 1 T=
   2 COUNT 2 T=
   3 COUNT 1 T=
   71 4 COUNT 3 T= 71 T=
   9 DATA 4 cells + !
   4 COUNT 2 T=
   T-REPORT ;

RUN
;package
