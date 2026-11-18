---
title: "Delete the linker's header-store boundary rejection"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T21:52:26.082345+01:00"
blocks:
  - habu-checked-src-reads-4f721c9b
---

Problem: src/habu/linker.f:2385 and :2406 reject a build's header store off a header boundary. Its last test input (test/fixtures/file-objects/straddle.f, STOP-STRADDLE) is gone, but a program file still stores into a header cell through a stepped pointer (`NULL-PTR DATA-VA + $2C48 +`), which habu-checked-src-reads-4f721c9b refuses. Acceptance: once no program file steps a pointer, the rejection and the code only it uses are gone, and the engine is a fixpoint. Depends: habu-checked-src-reads-4f721c9b. Claim: unassigned.
