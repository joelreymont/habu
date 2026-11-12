---
title: Map O_NONBLOCK on the x86-64 Linux open primitive
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:22:13.792241+02:00"
---

Problem: src/os/linux-x86-64/sys.f:184 OS-OPEN-FLAGS translates the BSD-shaped open flags (access mode, O_APPEND, O_CREAT, O_TRUNC, O_NOCTTY) to Linux values and drops O_NONBLOCK (BSD 4, Linux $800), which the arm64 mapping carries, so src/lib/test/fifo.f READ-END (:43) blocks on x86-64 Linux until a writer appears. Edits: after the O_NOCTTY line (sys.f:190) insert `   RSI 4 $800 OS-FLAG-BIT,                       \ O_NONBLOCK`; the comment's "four flags move" (:178) becomes "five flags move"; test/x86-64-emit.f:162 label "moves four flag bits" becomes "moves five flag bits" with its expected bytes extended. Acceptance: the x86-64 mapping carries bit 4 to $800 the way the arm64 one does; the FIFO tests (test/capture-tree-test.f, test/gate-signal-test.f, test/check-signal-test.f, test/db/pg-kill-test.f) pass on the x86-64 Linux host. Depends: habu-re-enable-the-959502b5 (x86-64 tests re-enabled). Claim: unassigned.
