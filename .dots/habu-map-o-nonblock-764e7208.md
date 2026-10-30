---
title: Map O_NONBLOCK on the x86-64 Linux open primitive
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:22:13.792241+02:00"
---

Problem: src/os/linux-x86-64/sys.f:185 OS-OPEN-FLAGS translates the BSD-shaped open flags (access mode, O_APPEND, O_CREAT, O_TRUNC, O_NOCTTY) to Linux values and drops O_NONBLOCK (BSD 4, Linux $800) exactly as the arm64 mapping did before habu-fix-o-nonblock-f18448d5, so src/lib/test/fifo.f READ-END blocks on x86-64 Linux until a writer appears. Acceptance: the x86-64 mapping carries bit 4 to $800 the way the arm64 one does; the FIFO rows (test/capture-tree-test.f, test/gate-signal-test.f, test/check-signal-test.f, test/db/pg-kill-test.f) pass on the x86-64 Linux host. Files: src/os/linux-x86-64/sys.f. Verify: on joel@pulsar-ua-106, an engine built from the tree runs the four rows rc 0 and READ-END returns with no writer. Depends: habu-fix-o-nonblock-f18448d5 (FS:O-NONBLOCK). Ownership: src/os/linux-x86-64/sys.f. Claim: unassigned.
