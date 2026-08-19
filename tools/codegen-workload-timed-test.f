\ codegen-workload-timed-test.f - the workload harness assertions that turn on a
\ clock. Run BY HAND, on a quiet machine:
\ bin/hb --load tools/codegen-workload-timed-test.f
\ One concern: the claims about a row's two columns that need the arms to have
\ taken measurably different amounts of time.
\
\ WHY THIS IS NOT SCHEDULED. tools/codegen-workload-test.f states, at its head,
\ that no assertion in it reads a clock, and that is what makes it safe to run
\ beside seven other suites. The same split is already in force for the code
\ judge: tools/judge-timed.f holds the cost-direction claims
\ its scheduled suite may not make, after a nine per cent margin failed one run
\ in ten idle and three in ten under load (dot habu-retire-the-flaky-25a37a74).
\
\ WHY THESE TWO ARE NOT SIMPLY DROPPED. A store that put the before-arm's time
\ in the after-arm's column, or a delta that read a row's two times in the other
\ order, inverts the sign of every verdict the report prints. The scheduled suite
\ catches an arm swap by the ANSWERS the two arms carry, because those are not
\ measurements - but a swap of the TIMES alone leaves the answers where they
\ were, and two numbers from one clock carry nothing that says which arm each
\ came from. The only way to tell them apart is to make the arms do very
\ different amounts of work, and that is a measurement.
\
\ WHAT MAKES THEM SAFE TO ASSERT ANYWAY is the margin. These are not "within
\ nine per cent" claims; the old arm does thousands of times the new arm's work
\ and the assertions are about which of the two recorded times is larger. A host
\ that could close that gap would have to make a loop of four thousand iterations
\ cost less than an empty one.

require tools/codegen-workload-test.f

CODEGEN-WORKLOAD-TEST:TIMED
