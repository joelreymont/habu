\ codegen-workload.f - the end-to-end workload measurement, run by hand.
\ One concern: the entry.
\
\   bin/hb --load tools/codegen-workload.f
\
\ WHAT IT ANSWERS. The four committed microbenchmark tables say the native chain
\ emits smaller and faster code for a pinned corpus of words. This says what that
\ is worth to a PROGRAM: it publishes one program twice in one process, once
\ through each code generator, runs the same workloads against both, and prints
\ the deltas with the two rows that decide whether each delta is readable at all.
\
\ WHAT IT IS NOT. It is not a gate and nothing here fails. The gate that carries
\ this work is tools/codegen-workload-test.f, and every assertion in it is a fact
\ about compiled code rather than a number a busy host can move. Run this one on
\ a quiet machine, the way bin/hb --load tools/judge-timed.f is run.
\
\ Loading tools/codegen-workload-run.f is what does the work: it publishes the
\ subjects, compiles the before-arms, migrates, compiles the after-arms, and
\ measures the one row whose arms the migration itself separates. MEASURE runs
\ the other nine and PRINT renders all of them.

require tools/codegen-workload-run.f
require tools/codegen-workload-report.f

CODEGEN-RUN:MEASURE
CODEGEN-WREPORT:PRINT
