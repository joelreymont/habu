---
title: Seventeen old hb test processes run with no parent
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T11:33:21.504624+02:00"
---

Problem: seventeen `bin/hb` processes started 2026-10-28 run under init: pid 22961 `bin/hb --load src/lib/process-fork-test.f` (cwd the retired workspace .jj-ws.noindex/tim-spawnsup; the file is gone from master) and 61474-61489 `bin/hb --load a64-ref.f test/process-pty-io-smoke.f` (cwd /private/tmp/a64ref/tree). Not determined: whether test/process-pty-io-smoke.f on master still leaves a process behind when its parent ends. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the seventeen are stopped (`orphans --kill` is Joel's call); a run of test/process-pty-io-smoke.f on master, ended normally and ended by a signal, leaves no process, or the leak has its own dot and fix. Files: test/process-pty-io-smoke.f, src/lib/process.f. Verify: ps before and after the run. Depends: none. Ownership: the process tests. Claim: unassigned.

Checked 2026-11-10 (bob) on master 24f12925 with the installed hb: `hb --load test/process-pty-io-smoke.f` ended normally (rc 0, 0.97 s) and killed with SIGKILL at 0.2, 0.4 and 0.6 s leaves no process 12 s later. Not covered: other kill times, Linux. The sixteen ran `a64-ref.f` first from /private/tmp/a64ref/tree, a tree master does not hold. What remains is stopping the seventeen.
