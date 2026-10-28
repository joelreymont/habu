---
title: Install T0 as the x86-64 runtime compiler
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.583786+02:00"
blocks:
  - habu-give-t0-one-fe9f04ec
  - habu-publish-and-shadow-8ea8909d
---

Problem: x86 engines compile every definition with tier 1. Acceptance: T0:COMPILE ( ptr u8 n -- ) lifted from compiler.f STAGE/RUN/RECORD (check, NFETCH:CAPTURE, RETRACT/report); T0:COMPILE-FIXED ( n n -- ) for constant/create/defer incl. PATCH-SLOT and defer trailer; does> clause unit; CB-ENTRY (x86 callbacks are new kernel work, kernel-x64.f:4686-4697); T0:INSTALL; runtime-x64.f installs T0; definers.f tier rules dropped. A committed test loads T0 through the real load path. Files: src/habu/t0.f, src/habu/t0-x64.f, src/habu/runtime-x64.f, src/habu/definers.f, kernel-x64.f callback rows. Verify: x86 peer bin/hb --load test/run.f. Depends: L1, L3. Ownership: those files. Claim: unassigned.
