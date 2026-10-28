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

Census (jerry-t0-l1/.out/l4-census.md, at styvunzx + ktmkppmt), lead rulings:
- The unit step T0:COMPILE needs (STAGE/RUN, check, refusals, reports, RETRACT, KEEP-ARITY, PUBLISH-IT kinds, PUBLISHED-ROWS, does> split state, NHOST source facts) moves out of NCOMP-private compiler.f into one shared package both compilers require; it survives tier 1. compiler.f joins this lane's files.
- x86 fixed path: definers.f 807/1011 branch on the target as 760 already does (no engine header cell); the branch goes when arm64 takes T0.
- "definers.f tier rules dropped" moves to L8: x86 always runs tier 1 (boot-x64.f:283), the tier-0 refusal is unreachable there and arm64 rows test it (outer-interpret.f 1283-1296).
- T0 needs no tape or IR context; it arms its own no-op tape observer so the checker records quotation arities, and counts tokens as the checker does.
Split: C1 kernel x86 `callback-entry -1` answers CB-THUNK (peer); C2 emitter fixed/defer/does>/CB-ENTRY rows (arm64 cross-emit harness, committed); C3 front end T0:COMPILE/COMPILE-FIXED/does>/callbacks/DISPATCH!/INSTALL + committed suite (peer); C4 runtime-x64.f installs T0 + definers.f fixed path (peer full suite + multi-generation convergence). Depends on L3-C3.
