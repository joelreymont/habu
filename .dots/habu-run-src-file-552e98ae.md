---
title: Run source-file-run the same way on both machines
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T15:56:16.039617+02:00"
---

Problem: arm64 runs a loaded file's unit on a fresh pool stack of one page (src/habu/habu1.f B-SOURCE-FILE-RUN, after the interp-range lane: CLOSED-FREE-CELL pool or STACK-GUARD:EMIT-MAP, BASE to that stack, CAP PAGE-BYTES), while x86-64 runs it on the caller's extent with the cursor as its floor (src/habu/kernel-x64.f SOURCE-FILE-RUN,). A file's stack room and its fault reports differ by machine (src/habu/crash.f's floor rule holds only on x86-64). Found by the interp-range re-review. Acceptance: one rule for both machines, chosen when x86-64 joins the shared layer; crash.f and both bodies' comments state it once. Files: src/habu/habu1.f, src/habu/kernel-x64.f, src/habu/crash.f. Verify: the x86-64 kernel tests and the suite on both hosts. Depends: habu-build-x86-64-3ed6f45d (the x86-64 step). Ownership: none. Claim: unassigned.
