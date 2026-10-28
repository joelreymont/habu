---
title: Seal T0 x86-64 output into NEMIT
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.559078+02:00"
blocks:
  - habu-give-t0-one-fe9f04ec
---

Problem: the x86 T0 emitter keeps its rows private; publication and capture read tier 1's NART emission. Acceptance: src/compiler/native/emission.f NEMIT record (OPEN bytes size ret arch, PLACE, FUNCTION+, CALL-SITE+, ADDR-SITE+, PATCH-SLOT!, SEAL, CLEAR) per umbrella section 2; NEMIT:ADDR-DATA=1 / ADDR-CODE=2 and every HIR:/x64ir/a64ir ADDR-* reader moved to them in this lane (no alias); x86 T0 SEAL fills NEMIT. Files: src/compiler/native/emission.f, src/habu/t0-x64.f, readers of HIR:ADDR-* (rg). Verify: L1 harness rows; x86 peer (x86-host) T0 rows. Depends: L1. Ownership: those files. Claim: unassigned.

L2b (jerry, .jj-ws.noindex/jerry-t0-l1 on mnpmzrsn): the seam owns the rows (UNIT+, CALL-ROW+, ADDR-ROW+); cells BYTES and PATCH replace CODE$ and the row readers; seam SEAL ( at ) and SEAL-UNPLACED file NEMIT (impl 0, loc = calling token offset, a quotation as ADDR-CODE); CALL/TAIL take loc. Stack order: seam, L2a (vnpwyyqm), L5a (mnpmzrsn), L2b.
