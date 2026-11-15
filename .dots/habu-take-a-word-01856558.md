---
title: "Take a word's type from its definition"
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.825097+02:00"
---

Problem: the building hb checks every definition before the check hook and records its row as facts text at the definition (src/core/check-hook.f CHECK-FACTS, src/core/checker.f FACTS! and PLACE-FACTS; the built engine reads it back through LIVE-BIND), so no word needs a row written elsewhere. Yet src/core carries 19 PRIM:/PPRIM: lines (11 in cell-effects.f, 6 in layout-buffer.f, 1 each in sumtype.f and arena.f) and cell-effects.f's TRUST rows, and comments that say a pre-hook word has no row or name a from-source prefix boot no route runs (cell-effects.f, pointer-storage.f:60, checker-owner-guard.f:2, src/compiler/native/checker-owner.f:19 and :72). An engine built without cell-effects.f's PRIM:/PPRIM: rows is byte-identical to one built with them; without its TRUST rows as well it converges, and only the facts of CELL, SIG-RAW-MODE and the 8 storage definers change.
Ruling (Joel, 2026-10-08): the definition is the source of a word's type. The kernel is the words compiled before the type checker runs; it is untyped and implicitly trusted. A kernel word that checked code calls carries a trusted type declared at its definition; every other kernel word needs no type, sits in the system package's private section and is stripped from the delivered hb. No census or certification ceremony; integration tests cover the kernel.
Acceptance: the PRIM:/PPRIM: rows for source-defined words and the PRIM:/PPRIM: definers are deleted with the comments that explain them (engine machine-code primitives keep their single row in src/habu/prims.f); kernel words checked code does not call carry no type and are absent from bin/hb.names.
Files: src/core/checker.f, src/core/util.f and the other prefix files whose words have rows, src/habu/prims.f only if needed.
Verify: full suite on the built hb.
Depends: none. Ownership: unassigned. Claim: unassigned.
