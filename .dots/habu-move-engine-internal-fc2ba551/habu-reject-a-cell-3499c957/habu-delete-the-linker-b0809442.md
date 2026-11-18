---
title: "Delete the linker's header-store boundary rejection"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T21:52:26.082345+01:00"
blocks:
  - habu-keep-the-data-a062de26
---

Problem: src/habu/linker.f:2385 and :2406 reject a build's header store off a header boundary. Once data-base is private no program file can store into a header cell, and the stack deletes its last input (test/fixtures/file-objects/straddle.f, STOP-STRADDLE), so no build reaches it. Acceptance: the rejection and the code only it uses are gone, and the engine is a fixpoint. Depends: habu-keep-the-data-a062de26. Claim: unassigned.
