---
title: RCA snapshot trailer corruption in owner-wid gate
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T19:56:11.811447+02:00"
---

Why: test/owner-wid-internal.f (SUITE owner-wid-internal in test/gate-stdlib-cases.f) fails identically on current master 22a3d104, the census parent, and the FILEMAP candidate: the compiler fixpoint and snapshot build report success, then the expected-good generated hb-new --load test/owner-wid-state.f exits 79 with hb: snapshot trailer corrupt and 22 assertions fail. The FILEMAP diff was proven non-causal by tri-tree comparison with private temp roots, so master carries a latent red gate: either a real engine defect in snapshot trailer writing/reading or a broken gate harness, and either way the exit-79 path was reached by an expected-good input. Exact result: root-cause with debugger evidence first per docs/debugging.md (baked stepper, snapshot/image dumpers, gdb catch on the exit path to read the throw site), not print-bisection; classify build-side versus load-side corruption; produce a minimal reproducer; fix the engine or harness defect; add a regression that loads a freshly built snapshot through the exact generated hb-new path. Owner: engine snapshot machinery (src/habu) with the owning gate test/owner-wid-internal.f. Dependencies: none - this red predates the current waves. Acceptance: owner-wid-internal exits 0 on the exact fixed tree; the regression fails on the pre-fix tree; the trailer defect class has a checker/engine guard or a documented structural reason it cannot recur. Forbidden: shrinking or bypassing the gate, blessing the corrupt trailer, calling it too expensive.
