---
title: Decide the tier-0 inline arm
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T19:05:01.198655+03:00"
---

Problem: src/habu/habu2.f C-CALL takes the BL arm unconditionally (its comment: the inline arm is off and tier 0 always calls, pending proof that the scan's instruction set covers everything a64emit.f can emit), while C-CALL-SCAN-SAFE and C-CALL-COPY-INLINE are retained with no caller; test/aot-band-data.f's DATA cases relied on the inline copy to carry a pre-window DATA address into a window word and so tested a mechanism the engine no longer runs (measured 2026-09-12 by the capture-window lane; the fixture is being moved to a vehicle the engine emits today). Acceptance: one decision recorded and implemented: either the inline arm returns with a scan proven to cover a64emit.f's instruction set (a differential over every emitter form, refusing by name on an unknown encoding) and the band fixtures regain the inline vehicle as a second case, or the arm and its two words are deleted with the comment and the docs updated. Decide after the tier stack lands (habu-build-the-compiler-c348eab0): with tier 1 the default, tier-0 inlining's value is the measurement that settles it. Files: src/habu/habu2.f, test/aot-band-data.f, docs/. Verify: the band suites, test/run.f, the compile floor if the arm returns. Depends: habu-build-the-compiler-c348eab0. Ownership: hazel. Claim: unassigned.
