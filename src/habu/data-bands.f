\ data-bands.f - the protected DATA bands, as one table, package DATA-BANDS.
\ Build-only, as data-claims.f is: runtime images need only layout.f.
\
\ Both engines' span guards read this table: the ARM64 GUARD-SPAN
\ (src/habu/habu1.f) and the x86-64 (PROT-SPAN) helper (src/habu/kernel-x64.f).
\ Each reads it twice - once for the hull [LO, HI) its bounding test compares
\ against, once to emit the per-band interval tests - so a band added here
\ widens both targets' bounding tests in the same edit that adds their band
\ tests, which a hull written out beside the list, or a copy of the rows per
\ target, could not promise. The zero-length row ends every walk: no count sits
\ beside the table to fall out of step with its rows. bootstrap/cg/forth.fs
\ mirrors the five bands its stage0 engine owns.
require src/habu/layout.f
require src/habu/data-claims.f

package DATA-BANDS

create TAB
   FRIEND-ARENA ,                 FRIEND-ARENA-LEN ,
   PROT-REG-OFF ,                 PROT-REG-LEN ,
   ENGINE-HOOK-OFF ,              ENGINE-HOOK-LEN ,
   NCOMP-DISPATCH:TIER-CELL ,     1 cells ,
   NCOMP-DISPATCH:DEF-TIER-CELL , 3 cells ,
   TIER-PROV:OPEN-CELL ,          TIER-PROV:END TIER-PROV:OPEN-CELL - ,
   UNIT-COMPILE-CELL ,            1 cells ,
   BODYBUF-OFF ,                  BODYBUF-CAP 2 + ,
   TXN-STATE-OFF ,                TXN-STATE-LEN ,
   0 ,                            0 ,

public

\ Row ix's DATA offset and byte length. The row whose length is zero ends the
\ table.
: OFF ( n -- n ) {: ix:n :} ix 2 * cells TAB + @ ;
: LEN ( n -- n ) {: ix:n :} ix 2 * 1 + cells TAB + @ ;

\ The hull: the lowest band base and the highest band end over the whole table.
\ Each opens on the first row and widens by every later one, so it needs no
\ sentinel start value that a band could one day sit outside of.
: LO ( -- n )
   0 OFF  1 BEGIN dup LEN 0 <> WHILE
      dup OFF rot min swap  1+
   REPEAT drop ;

: HI ( -- n )
   0 OFF 0 LEN +  1 BEGIN dup LEN 0 <> WHILE
      dup OFF over LEN + rot max swap  1+
   REPEAT drop ;

private

\ EVERY GUARDED BAND MUST BE A DECLARED CLAIM, start and length both. The
\ transaction row guarded TXN-STATE-LEN when that constant was $3000 and the
\ transaction's own cells ended after $300, so PROT-GUARD refused stores across
\ 11520 bytes no claim owned - which is exactly what kept the task-user arena
\ out of the only run in the per-task header big enough to hold it. Nothing
\ compared the guard against the map until this did. It runs when an engine
\ builder loads this file and dies, so a band that outgrows or outlives its
\ claim cannot ship.
: CLAIM-AT ( n -- n ) {: ix:n :}       \ the claim at this band's offset, else -1
   DATA-CLAIMS:COUNT-ROWS 0 ?do
      i DATA-CLAIMS:ROW-OFF ix OFF = if i unloop exit then
   loop -1 ;

: DECLARED-AT ( n -- ) {: ix:n :}
   ix CLAIM-AT {: hit:n :}
   hit 0 < if
      s" data-bands: PROT-GUARD band has no DATA-CLAIMS claim at its offset" 76 die
   then
   hit DATA-CLAIMS:ROW-LEN ix LEN <> if
      DATA-CLAIMS:MSG-RESET
      s" data-bands: PROT-GUARD band length differs from its claim: " DATA-CLAIMS:MSG+
      hit DATA-CLAIMS:NAME-AT DATA-CLAIMS:MSG+
      DATA-CLAIMS:MSG$ 76 die
   then ;

: DECLARED ( -- )
   0 BEGIN dup LEN 0 <> WHILE  dup DECLARED-AT  1+  REPEAT drop ;

DECLARED

;package
