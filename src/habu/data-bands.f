\ data-bands.f - every protected DATA band is a declared claim, package
\ BAND-CLAIMS. Build-only, as data-claims.f is: the bands are
\ src/habu/layout.f's DATA-BANDS table, which every engine's prefix carries,
\ and this file checks them against data-claims.f's map when an engine builder
\ (src/habu/habu1.f, src/habu/kernel-x64.f) loads it. DATA-BANDS is sealed
\ with the engine that bakes it, so the check is a package of its own.
require src/habu/layout.f
require src/habu/data-claims.f

package BAND-CLAIMS

\ EVERY GUARDED BAND MUST BE A DECLARED CLAIM, start and length both. The
\ transaction row guarded TXN-STATE-LEN when that constant was $3000 and the
\ transaction's own cells ended after $300, so PROT-GUARD refused stores across
\ 11520 bytes no claim owned - which is exactly what kept the task-user arena
\ out of the only run in the per-task header big enough to hold it. Nothing
\ compared the guard against the map until this did. It runs when an engine
\ builder loads this file and dies, so a band that outgrows or outlives its
\ claim cannot ship.

\ The claim at row ix's offset, else -1.
: CLAIM-AT ( n -- n )
   {: ix:n :}
   DATA-CLAIMS:COUNT-ROWS 0 ?do
      i DATA-CLAIMS:ROW-OFF ix DATA-BANDS:OFF = if i unloop exit then
   loop -1 ;

: DECLARED-AT ( n -- )
   {: ix:n :}
   ix CLAIM-AT {: hit:n :}
   hit 0 < if
      s" data-bands: PROT-GUARD band has no DATA-CLAIMS claim at its offset" 76 die
   then
   hit DATA-CLAIMS:ROW-LEN ix DATA-BANDS:LEN <> if
      DATA-CLAIMS:MSG-RESET
      s" data-bands: PROT-GUARD band length differs from its claim: " DATA-CLAIMS:MSG+
      hit DATA-CLAIMS:NAME-AT DATA-CLAIMS:MSG+
      DATA-CLAIMS:MSG$ 76 die
   then ;

: DECLARED ( -- )
   0 BEGIN dup DATA-BANDS:LEN 0 <> WHILE  dup DECLARED-AT  1+  REPEAT drop ;

DECLARED

;package
