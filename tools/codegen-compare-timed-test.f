\ codegen-compare-timed-test.f - the harness assertions that turn on a clock.
\ Run BY HAND, on a quiet machine: bin/hb --load tools/codegen-compare-timed-test.f
\ One concern: the claims about cost, which no gate may make.
\
\ WHY THESE ARE NOT SCHEDULED. A cost is a measurement, and a scheduled run
\ happens while seven other suites have the cores. The third corpus's T-SUM row
\ was asserted "not costlier" at a margin of nine per cent and failed one run in
\ ten with the machine idle, three in ten under load (dot
\ habu-retire-the-flaky-25a37a74). tools/codegen-compare-test.f, which IS
\ scheduled, now contains no assertion that reads one measurement against
\ another; the ones that do are here, where the precondition they need is
\ something the person running them can supply.
\
\ WHY THEY ARE NOT SIMPLY DELETED. "The chain's code is not slower than the
\ engine's" is the claim the whole comparison exists to make, and dropping it
\ would have replaced a flake with a silence.
\
\ WHAT THE GATE KEEPS INSTEAD is the data-stack traffic each column's emitted
\ code makes, pinned row by row in tools/codegen-compare-test.f. That is the
\ structural fact the cost difference rests on, and it is not the same claim:
\ the third corpus's SGD row makes ONE MORE data-stack access than the engine's
\ code for the same body and is not slower. Both are kept because of that gap.
\
\ RUN THIS BESIDE bin/hb --load tools/codegen-compare.f, the other half of the
\ timed check: that entry compares every row's cost with its committed table and
\ prints the rows the new column costs more on. This one asserts the direction.

require tools/codegen-compare-test.f

CODEGEN-COMPARE-TEST:TIMED
