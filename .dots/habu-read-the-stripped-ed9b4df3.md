---
title: "Read the stripped startup's shapes from one description"
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T09:02:57.336034+03:00"
---

Problem (found by the reach lane, landed 1d46de1f): test/gate-aot-image.f (DATA-RESTORE?, CHECK-COPY, CHECK-CURSORS, CHECK-VGET) and tools/image-size-lib.f (TEXT-ADR9?, TEXT-ADR9-TARGET, FIND-BLOB and the x11 movz/movk readers) each carry their own copy of the stripped startup's instruction shapes. Converting the blob's address from one ADR x9 to the four-word TEXT-ADR, sequence had to be done twice, and the second reader failed silently: image-size-lib's guard counted zero ADR x9, took the code-only path and reported 'data 0 written' for every stripped image until HBT-SIZE-AOT's DATA-WRITTEN assertion caught it. Acceptance: one shape description both readers require (a module under tools/ or test/ that names each startup sequence once, or the emitter in src/habu/aot-lib.f publishing its shapes), so a startup change is made in one place and both readers follow; a shape change not mirrored fails the gate by name rather than through a size assertion; test/gate-aot-image.f, tools/engine-size-test.f and tools/hb-build-test.f green. Files: test/gate-aot-image.f, tools/image-size-lib.f, src/habu/aot-lib.f. Verify: those three suites; test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
