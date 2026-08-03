\ codegen-compare-cli.f - what a codegen comparison run does.
\ One concern: the runs a caller can ask for, and the exit status.
\
\ Kept apart from tools/codegen-compare.f so that the check a person runs by
\ hand and the check the scheduled test runs are the same code, and so that
\ loading the harness can never write a file. The entry file reads the command
\ line and calls one of these; nothing here looks at the command line.
\
\ Three runs: CHECK compares everything, CHECK-EXACT compares everything except
\ the timings, and UPDATE rewrites the committed tables. CHECK and CHECK-EXACT
\ differ by one decision, taken in one place, over one shared body. Each of them
\ measures every pinned corpus in a pass of its own, in the order the corpora
\ were written - there are four.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require tools/codegen-compare-cases.f
require tools/codegen-compare-cases2.f
require tools/codegen-compare-cases3.f
require tools/codegen-compare-cases4.f
require tools/codegen-compare-report.f
require tools/codegen-compare-baseline.f
require tools/codegen-compare-new.f
require tools/codegen-compare-new2.f
require tools/codegen-compare-new3.f
require tools/codegen-compare-new4.f

package CODEGEN-COMPARE-CLI

private

70 constant FINDINGS-RC           \ sysexits EX_SOFTWARE: the comparison disagreed

\ One measurement pass, one report, one finding count: the whole check apart
\ from the decision about the cost column, which the two public entries below
\ make before calling this. Factored so the checked and the unchecked run are
\ the same code on the same load path and cannot drift apart.

\ The two empty calls are a timing like any other, so the run that leaves the
\ cost column out leaves this out too - on a host with every core busy the two
\ floors can drift apart for the same reason a row's cost can. It is the timed
\ check, run by hand, that says the two columns are still entered the same way.
: FLOOR-FINDINGS ( -- n )
   CODEGEN-BASELINE:COSTS-CHECKED? 0= if 0 exit then
   CODEGEN-REPORT:SAY-FLOOR-GAP ;

\ One measured pass reported and compared with its own committed table. The
\ caller runs the pass; this reads whatever the pass left in the store, which is
\ why the two corpora need no second copy of any of it.
: TABLE-FINDINGS ( ptr u8 n -- n ) {: a:ptr u:n :}
   CODEGEN-REPORT:PRINT
   a u CODEGEN-BASELINE:LOAD
   CODEGEN-BASELINE:COMPARE
   CODEGEN-REPORT:SAY-MISMATCHES +
   FLOOR-FINDINGS + ;

\ ALL FOUR TABLES, IN ORDER, AND THE FINDINGS ADDED UP. Each corpus is measured
\ in a pass of its own - the store holds one corpus at a time, so each later pass
\ resets it - and each is compared with its own committed table. A run that
\ stopped after the first table would leave the others unchecked while still
\ printing "0 finding(s)", so the four are added and none can hide another.
: RUN-CHECK ( -- n )
   CODEGEN-CASES:RUN
   CODEGEN-CASES:BASELINE-PATH$ TABLE-FINDINGS
   CODEGEN-CASES2:RUN
   CODEGEN-CASES2:BASELINE-PATH$ TABLE-FINDINGS +
   CODEGEN-CASES3:RUN
   CODEGEN-CASES3:BASELINE-PATH$ TABLE-FINDINGS +
   CODEGEN-CASES4:RUN
   CODEGEN-CASES4:BASELINE-PATH$ TABLE-FINDINGS + ;

: VERDICT ( n -- ) {: findings:n :}
   cr
   s" codegen-compare: " type findings FMT:.U s"  finding(s)" type cr
   findings 0= if exit then
   S\" codegen-compare: the measurement disagrees with the committed baseline, or the two code generators disagree\n"
   FINDINGS-RC die ;

public

\ Measure the pinned corpus with both code generators, print the report, and
\ check two things: that the old column still agrees with the committed
\ baseline, and that every word the new chain compiled computes what the old
\ one computes. Exits the process non-zero, after naming every disagreement,
\ when either finds anything.
: CHECK ( -- )
   CODEGEN-BASELINE:COSTS-CHECKED!
   RUN-CHECK VERDICT ;

\ The same run with the cost column and the pass budget left out, for a caller
\ that runs under a gate. A cost is the one column that is a measurement rather
\ than a fact about the compiled code, and a measurement taken while seven other
\ suites have the cores can miss the tolerance band for reasons that have nothing
\ to do with a code generator - measured, with the numbers, at the head of
\ tools/codegen-compare-baseline.f. Sizes, outputs, the head-to-head agreement of
\ the two code generators and the structure of the committed table are all still
\ compared exactly, and the run prints one line saying the timings were not.
: CHECK-EXACT ( -- )
   CODEGEN-BASELINE:COSTS-UNCHECKED!
   RUN-CHECK VERDICT ;

\ Measure, print, and rewrite one committed table from this measurement. The
\ caller runs the pass, as with TABLE-FINDINGS.
: WRITE-TABLE ( ptr u8 n ptr u8 n -- ) {: ba:ptr bu:n ca:ptr cu:n :}
   CODEGEN-REPORT:PRINT
   ba bu  ca cu CODEGEN-REPORT:BASELINE$  ATOMIC-WRITE-FILE
   cr s" codegen-compare: baseline rewritten: " type
   ba bu type cr ;

\ Measure, print, and rewrite every committed table from this measurement.
: UPDATE ( -- )
   CODEGEN-CASES:RUN
   CODEGEN-CASES:BASELINE-PATH$ CODEGEN-CASES:CORPUS-PATH$ WRITE-TABLE
   CODEGEN-CASES2:RUN
   CODEGEN-CASES2:BASELINE-PATH$ CODEGEN-CASES2:CORPUS-PATH$ WRITE-TABLE
   CODEGEN-CASES3:RUN
   CODEGEN-CASES3:BASELINE-PATH$ CODEGEN-CASES3:CORPUS-PATH$ WRITE-TABLE
   CODEGEN-CASES4:RUN
   CODEGEN-CASES4:BASELINE-PATH$ CODEGEN-CASES4:CORPUS-PATH$ WRITE-TABLE ;

;package
