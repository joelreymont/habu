\ judge-timed.f - the judge's assertions that turn on a clock.
\ Run BY HAND, on a quiet machine: bin/hb --load tools/judge-timed.f
\ One concern: the claim about cost, which no gate may make.
\
\ THE CLAIM. The chain's code is not slower than the engine's, row by row. It is
\ what the whole comparison exists to say and nothing in the judged artifact can
\ carry it: a byte count is not a cost, the data-stack traffic beside it is not a
\ cost either, and a compiler that emitted fewer, cheaper-looking instructions
\ and took longer would pass every column of that table.
\
\ WHY IT IS NOT SCHEDULED. A cost is a measurement and a scheduled run happens
\ while seven other suites have the cores. The comparison this judge replaces
\ learnt that the hard way: a cost-direction assertion on the third corpus's
\ T-SUM row failed one scheduled run in ten with the machine idle and three in
\ ten under load (dot habu-retire-the-flaky-25a37a74), and the claim moved to a
\ hand-run entry that no suite lists. This is that entry, and it is listed in no
\ suite for the same reason. tools/judge-test.f, which IS scheduled, makes no
\ assertion that reads a clock.
\
\ WHY IT IS NOT SIMPLY DROPPED. "The chain's code is not slower than the
\ engine's" is the claim the campaign is about, and dropping it would replace a
\ flake with a silence. What the gate keeps in its place is the data-stack
\ traffic column of the artifact, which is the structural fact the cost
\ difference rests on - and the two are NOT the same claim, which is why both
\ are kept: one row of the table touches that stack more often than the engine's
\ code does and is far faster all the same.
\
\ HOW THE DIRECTION IS DECIDED, and where the band comes from. Both columns are
\ measured in ONE pass, in one process, interleaved, with the same measurement
\ floor taken off - the two habu columns are entered the same way, so they lose
\ the same constant. So the engine's own cost for the row IS the calibration and
\ nothing is compared against a number from another host or another day. The
\ band is the noise this run measured for itself: the widest gap between two
\ measurements of ONE program in this process. A difference smaller than that is
\ not a difference. It is not a constant chosen once - it widens by itself when
\ the host is loaded, because it is measured on that host in the same process.
\ tools/judge/row.f SLOWER? owns it and says there why the old harness's fixed
\ COST-BAND is not what is used here.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require tools/judge/check.f

package JUDGE-TIMED

private

: SAY-PICOS ( ptr u8 n n -- ) {: a:ptr u:n picos:n :}
   a u type picos FMT:.INT s"  ps" type ;

: SAY-SLOWER ( n -- ) {: k:n :}
   s" judge-timed: SLOWER " type k JUDGE-ROW:NAME$ type
   s"  chain " type k JUDGE-ROW:NEW-PICOS@ JUDGE-ROW:FLOOR@ - FMT:.INT
   s"  ps against engine " type k JUDGE-ROW:OLD-PICOS@ JUDGE-ROW:FLOOR@ - FMT:.INT
   s"  ps" type cr ;

: SAY-UNCOMPARABLE ( n -- ) {: k:n :}
   s" judge-timed: NO-RATIO " type k JUDGE-ROW:NAME$ type
   s"  the engine's cost is at or under the measurement's own floor" type cr ;

: OFFENDERS ( -- )
   JUDGE-ROW:ROWS 0 ?do
      i JUDGE-ROW:REFUSED? 0= if
         i JUDGE-ROW:COST-COMPARABLE? 0= if i SAY-UNCOMPARABLE then
         i JUDGE-ROW:SLOWER? if i SAY-SLOWER then
      then
   loop ;

: BAND-NOTE ( -- )
   s" judge-timed: band " type JUDGE-ROW:SPREAD@ FMT:.U
   s"  permille, measured this run as the widest gap between two" type cr
   s" judge-timed: measurements of one program; the floor measured " type
   JUDGE-ROW:FLOOR@ FMT:.INT s"  ps." type cr
   s" judge-timed: the whole cost table is bin/hb --load tools/judge.f" type cr ;

public

: MAIN ( -- )
   T-RESET
   JUDGE-CHECK:JUDGE-ALL
   BAND-NOTE
   OFFENDERS
   JUDGE-ROW:UNCOMPARABLE-ROWS 0 T=
   JUDGE-ROW:SLOWER-ROWS 0 T=
   T-REPORT ;

;package

JUDGE-TIMED:MAIN
