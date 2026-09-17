---
title: Bound the pty harness reap with a deadline
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T14:01:37.060022+03:00\""
---

Problem: every pty case in test/proc-pty.f and test/aot-data-span-forge.f reaps its child with PID @ >PID PROC-WAIT-RC (lib/process.f:133), which waits without a timeout, so a harness that stops draining the master wedges: measured 2026-09-17 (forge lane), a child blocked writing 5000 bytes into a pty nobody read and the parent sat in do_wait for 5 m 34 s until killed by hand; the suite timeout (360 s) would have killed the item without a diagnosis. Acceptance: the pty harnesses reap through a deadline-bounded wait (drain the master to hang-up first, then wait with the remaining PROC-LEFT-MS; on expiry kill the child and red the case naming the pid and the bytes still unread) and a regression case forces the wedge (child writes past the buffer, harness stops reading) and asserts the case reds within the deadline instead of hanging; PROC-WAIT-RC keeps its contract for callers that want an unbounded wait. Files: test/proc-pty.f, test/aot-data-span-forge.f, lib/process.f if a bounded reap word is added (lib/process-cmd*.f already own a timeout outcome to reuse). Verify: bin/hb --load lib/errors.f lib/process.f test/proc-pty.f; bin/hb --load test/aot-data-span-forge.f; the forced-wedge case. Depends: habu-share-one-pty-b1d88b7c (the reap moves into the shared harness; bound it there once). Ownership: test pty harnesses. Claim: agent=hazel-pty-reap workspace=.jj-ws/hazel-pty-reap.
