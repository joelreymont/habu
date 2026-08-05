\ codegen-compare-cli.f - what a codegen comparison run does.
\ One concern: the runs a caller can ask for, and the exit status.
\
\ Kept apart from tools/codegen-compare.f so that the check a person runs by
\ hand and the check the scheduled test runs are the same code, and so that
\ loading the harness can never write a file. The entry file reads the command
\ line and calls one of these; nothing here looks at the command line.
\
\ Three runs: CHECK compares everything, CHECK-EXACT compares everything except
\ the timings, and UPDATE-ONE rewrites one committed table. CHECK and
\ CHECK-EXACT differ by one decision, taken in one place, over one shared body.
\
\ EVERY DRIVER HERE WALKS tools/codegen-compare-corpora.f RATHER THAN NAMING THE
\ CORPORA. Each corpus declares its name, its committed table and its source
\ file beside its own cases, and the loops below measure whatever was declared,
\ in the order it was declared. Four hand-written pairs in the check and four
\ hand-written triples in the update were four places to forget a corpus in; a
\ run that stopped after the first table would have left the others unchecked
\ while still printing "0 finding(s)". The one thing the register cannot hold is
\ the word that runs a corpus's pass - the checker refuses to execute a routine
\ fetched from untyped memory, and rightly - so RUN-PASS below is where the
\ corpora are named, once, and an index it does not know throws rather than
\ measuring the wrong corpus.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require tools/codegen-compare-corpora.f
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

\ The measurement pass of one declared corpus. This is the one place the four
\ corpora are named, and the index is the register's own, so the pass that runs
\ and the table it is compared against cannot come from two different corpora.
\ A corpus declared without an arm here reaches the throw instead of being
\ measured as some other corpus.
: RUN-PASS ( n -- ) {: k:n :}
   k 0 = if CODEGEN-CASES:RUN exit then
   k 1 = if CODEGEN-CASES2:RUN exit then
   k 2 = if CODEGEN-CASES3:RUN exit then
   k 3 = if CODEGEN-CASES4:RUN exit then
   E-CODEGEN-COMPARE-CORPUS throw ;

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

\ One corpus measured, reported and compared with its own committed table.
: TABLE-FINDINGS ( n -- n ) {: k:n :}
   k RUN-PASS
   CODEGEN-REPORT:PRINT
   k CODEGEN-CORPORA:BASELINE$ CODEGEN-BASELINE:LOAD
   CODEGEN-BASELINE:COMPARE
   CODEGEN-REPORT:SAY-MISMATCHES +
   CODEGEN-REPORT:SAY-BYTE-LOSSES +
   FLOOR-FINDINGS + ;

\ EVERY DECLARED TABLE, IN ORDER, AND THE FINDINGS ADDED UP. Each corpus is
\ measured in a pass of its own - the store holds one corpus at a time, so each
\ later pass resets it - and each is compared with its own committed table. The
\ findings are added so that none can hide another.
: RUN-CHECK ( -- n )
   0
   CODEGEN-CORPORA:COUNT 0 ?do
      i TABLE-FINDINGS +
   loop ;

: VERDICT ( n -- ) {: findings:n :}
   cr
   s" codegen-compare: " type findings FMT:.U s"  finding(s)" type cr
   findings 0= if exit then
   S\" codegen-compare: the measurement disagrees with the committed baseline, or the two code generators disagree\n"
   FINDINGS-RC die ;

public

\ Measure the pinned corpus with both code generators, print the report, and
\ check three things: that the old column still agrees with the committed
\ baseline, that every word the new chain compiled computes what the old one
\ computes, and that none of them is more bytes of machine code than the
\ engine's, apart from the rows tools/codegen-compare-report.f names as known
\ losses with the dot each one dies with. Exits the process non-zero, after
\ naming every disagreement, when any of them finds anything.
: CHECK ( -- )
   CODEGEN-BASELINE:COSTS-CHECKED!
   RUN-CHECK VERDICT ;

\ The same run with the cost column and the pass budget left out, for a caller
\ that runs under a gate. A cost is the one column that is a measurement rather
\ than a fact about the compiled code, and a measurement taken while seven other
\ suites have the cores can miss the tolerance band for reasons that have nothing
\ to do with a code generator - measured, with the numbers, at the head of
\ tools/codegen-compare-baseline.f. Sizes, outputs, the head-to-head agreement of
\ the two code generators, the byte adjudication of the two columns against each
\ other and the structure of the committed table are all still compared exactly,
\ and the run prints one line saying the timings were not.
: CHECK-EXACT ( -- )
   CODEGEN-BASELINE:COSTS-UNCHECKED!
   RUN-CHECK VERDICT ;

\ Measure ONE declared corpus, print its report, and rewrite ITS committed table
\ from that measurement - and no other table. Regenerating a baseline used to
\ rewrite all four, which put three tables nobody had touched into a diff a
\ person was about to commit, and three lanes have restored them by hand.
: UPDATE-ONE ( n -- ) {: k:n :}
   k RUN-PASS
   CODEGEN-REPORT:PRINT
   k CODEGEN-CORPORA:BASELINE$
   k CODEGEN-REPORT:BASELINE$
   ATOMIC-WRITE-FILE
   cr s" codegen-compare: baseline rewritten: " type
   k CODEGEN-CORPORA:BASELINE$ type cr ;

\ Every declared table rewritten from one run. A caller has to ask for this by
\ name; see tools/codegen-compare.f for why it is not what a bare --update does.
: UPDATE-ALL ( -- )
   CODEGEN-CORPORA:COUNT 0 ?do i UPDATE-ONE loop ;

\ Rewrite the one table this name was declared for. Answers false, having
\ written nothing, when no corpus goes by that name - the caller says so and
\ names the ones that exist rather than guessing which table was meant.
: UPDATE-NAMED ( ptr u8 n -- bool )
   CODEGEN-CORPORA:FIND dup 0 < if drop false exit then
   UPDATE-ONE true ;

;package
