\ codegen-compare-cli.f - what a codegen comparison run does.
\ One concern: the two runs the command line offers, and the exit status.
\
\ Kept apart from tools/codegen-compare.f so that the check a person runs by
\ hand is the same word the scheduled test runs, and so that loading the harness
\ can never write a file. The entry file reads the command line and calls one of
\ these; nothing here looks at the command line.

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
   CODEGEN-CASES:RUN
   CODEGEN-REPORT:PRINT
   CODEGEN-BASELINE:PATH$ CODEGEN-BASELINE:LOAD
   CODEGEN-BASELINE:COMPARE
   CODEGEN-REPORT:SAY-MISMATCHES +
   VERDICT ;

\ Measure, print, and rewrite the committed baseline from this measurement.
: UPDATE ( -- )
   CODEGEN-CASES:RUN
   CODEGEN-REPORT:PRINT
   CODEGEN-BASELINE:PATH$ CODEGEN-REPORT:BASELINE$ ATOMIC-WRITE-FILE
   cr s" codegen-compare: baseline rewritten: " type
   CODEGEN-BASELINE:PATH$ type cr ;

;package
