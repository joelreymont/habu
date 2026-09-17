---
title: Guard CK-AOT-REG-INSTALL on the unseeded build path
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T13:17:55.800410+03:00"
---

Problem: the private-name strip removes CK-AOT-REG-INSTALL from product images; harmless today because habu2.f emits INSTALL, only when SEEDED-RUNTIME? is false, but nothing guards that coupling, so an unseeded build from a stripped engine would fail at run time with no build-time diagnostic (strip lane, 2026-09-17). Acceptance: either the unseeded path is exercised by a test that runs INSTALL, from a stripped engine, or the emitter refuses to emit INSTALL, when the capture would strip the name, with the reason in the source; docs state which. Files: src/habu/habu2.f, src/habu/aot-capture.f, test/. Verify: the test; fixpoint. Depends: none. Ownership: emitter. Claim: unassigned.
