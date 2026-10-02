\ Build checks remain live; their map and helper words do not ship in the runtime.
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

package DATA-CLAIMS-TEST

$1000 constant CAP
240000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot

: EXPECT ( ptr u8 n n ptr u8 n -- )
   {: src:ptr u:n rc:n msg:ptr msgu:n :}
   src u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc rc T-OUTCOME-EXITED=
   OUT outu LEN>N type
   ERR erru LEN>N msg msgu T$= ;

public
: RUN ( -- )
   T-RESET
   s" product has no build-only DATA-CLAIMS namespace" T-LABEL
   s" DATA-CLAIMS" XREF-NAMESPACE-WL XREF-FIND-WL XREF-FOUND? TFALSE

   s" overlapping claims still refuse and name both rows" T-LABEL
   s" require src/habu/data-claims.f HND-CELL DATA-CLAIMS:TAB ! DATA-CLAIMS:CLAIMS-ASSERT"
   76 S\" layout: DATA-CLAIMS overlap: DP-CELL and HND-CELL\n" EXPECT

   s" the image writer still checks the guarded band's declared extent" T-LABEL
   s" require src/habu/data-claims.f : DC-BAD-BAND ( -- ) DATA-CLAIMS:COUNT-ROWS 0 ?do i DATA-CLAIMS:ROW-OFF FRIEND-ARENA = if 1 DATA-CLAIMS:TAB i DATA-CLAIMS:ROW-CELLS * 1+ cells + ! unloop exit then loop ; DC-BAD-BAND require tools/native-emit.f"
   76 S\" data-bands: PROT-GUARD band length differs from its claim: FRIEND-ARENA\n" EXPECT

   \ A macOS host's DATA is far larger than the x86-64 target's, so only a band
   \ placed for the target's DATA-SIZE meets a cell moved to its first byte.
   s" an x86-64 build refuses a claim in the target's profiler band" T-LABEL
   s" require src/habu/data-claims.f require src/os/linux-x86-64/target-layout.f 0 X64LAYOUT:DATA-SIZE PROF-ABI:PROF-BAND-AT DATA-CLAIMS:TAB ! require test/x86-64-boot-harness.f"
   76 S\" layout: DATA-CLAIMS overlap: DP-CELL and PROF-BAND\n" EXPECT

   s" an x86-64 build admits a claim that ends where that band starts" T-LABEL
   s" require src/habu/data-claims.f require src/os/linux-x86-64/target-layout.f 0 X64LAYOUT:DATA-SIZE PROF-ABI:PROF-BAND-AT 1 cells - DATA-CLAIMS:TAB ! require test/x86-64-boot-harness.f"
   0 s" " EXPECT
   T-REPORT ;

;package

DATA-CLAIMS-TEST:RUN
