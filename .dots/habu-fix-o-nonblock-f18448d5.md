---
title: Fix O_NONBLOCK on the Linux open primitive
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.155581+02:00"
---

Problem: test/fifo.f READ-END opens the FIFO with the open primitive and O_NONBLOCK; the Linux primitive's flag mapping in src/os/linux/sys.f ignores O_NONBLOCK, so the open blocks until a writer appears. On Linux (spark) this hangs exit-ends-capture (src/lib/process-env-test.f), capture-tree-test, gate-signal-test, check-signal-test and pg-kill-test; found by timer lane 1, 2026-10-30. Acceptance: the Linux primitive honors O_NONBLOCK and the five rows pass on spark. Files: src/os/linux/sys.f, test/fifo.f. Verify: on spark, test/fifo.f opens READ-END with no writer present; the five rows green. Depends: none. Ownership: src/os/linux/sys.f. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nonblock.
