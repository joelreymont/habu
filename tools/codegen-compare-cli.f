\ codegen-compare-cli.f - what a codegen comparison run does.
\ One concern: the runs a caller can ask for, and the exit status.
\
\ Kept apart from tools/codegen-compare.f so that the check a person runs by
\ hand and the check the scheduled test runs are the same code, and so that
\ loading the harness can never write a file. The entry file reads the command
\ line and calls one of these; nothing here looks at the command line.
\
\ Three runs: CHECK compares everything, CHECK-EXACT compares everything except
\ the timings, and UPDATE rewrites the committed table. CHECK and CHECK-EXACT
\ differ by one decision, taken in one place, over one shared body.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require tools/codegen-compare-cases.f
require tools/codegen-compare-report.f
require tools/codegen-compare-baseline.f
require tools/codegen-compare-new.f

package CODEGEN-COMPARE-CLI

private

70 constant FINDINGS-RC           \ sysexits EX_SOFTWARE: the comparison disagreed

\ One measurement pass, one report, one finding count: the whole check apart
\ from the decision about the cost column, which the two public entries below
\ make before calling this. Factored so the checked and the unchecked run are
\ the same code on the same load path and cannot drift apart.
: RUN-CHECK ( -- n )
   CODEGEN-CASES:RUN
   CODEGEN-REPORT:PRINT
   CODEGEN-BASELINE:PATH$ CODEGEN-BASELINE:LOAD
   CODEGEN-BASELINE:COMPARE
   CODEGEN-REPORT:SAY-MISMATCHES + ;

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

\ Measure, print, and rewrite the committed baseline from this measurement.
: UPDATE ( -- )
   CODEGEN-CASES:RUN
   CODEGEN-REPORT:PRINT
   CODEGEN-BASELINE:PATH$ CODEGEN-REPORT:BASELINE$ ATOMIC-WRITE-FILE
   cr s" codegen-compare: baseline rewritten: " type
   CODEGEN-BASELINE:PATH$ type cr ;

;package
