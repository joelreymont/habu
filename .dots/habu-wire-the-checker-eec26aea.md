---
title: Wire the checker payload capture or retire it
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T10:53:21.380871+03:00"
---

Problem: the artifact's checker payload sections (S-SIGS, S-SIGSTR, S-REG in src/habu/aot-file.f) always travel empty because nothing in production arms or marks the collection: ACAP-AUDIT-SIGS is uncalled (AOT-CAPTURE:CAPTURE does not run it), CHECKER-ASIG-ARM is called only from two test files, CHECKER-REG-AOT-MARK from nothing, and the comments at src/core/checker.f:5672,6753,9032 and src/core/type-family.f:2951 claim aot-arm.f arms and marks; AOT-ARM:SIG-CLOSE (defined 2026-09-13) is therefore a guarded no-op until a producer arms the pool. Measured 2026-09-13 by the S-PWID lane. Acceptance: one decision, implemented: either the capture arms the pool and marks the registry across the window so the three sections carry the window's signatures and types and the seeded engine's registry base is measured against them (with a regression that captures a window declaring a family and reads it back from the artifact), or the three sections, the arming words and the claims are retired with a version bump. Files: src/habu/aot-capture.f, src/habu/aot-arm.f, src/habu/aot-file.f, src/core/checker.f, src/core/type-family.f, test/. Verify: the round-trip fixture, the chain, test/run.f. Depends: habu-retire-the-s-4fbc244f. Ownership: hazel. Claim: unassigned.
