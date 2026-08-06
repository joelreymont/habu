---
title: Key A64RAV interference on register file, not class
status: open
priority: 1
issue-type: task
created-at: "2026-08-06T20:14:41.594191+02:00"
---

SOUNDNESS PREREQUISITE for any third register class; must land before a vector class exists.

src/compiler/native/regalloc-verify.f:600-608 OVERLAP-CK guards with 'j CLS-AT i CLS-AT =' - literal class-code equality - so two values of DIFFERENT classes are never compared for register overlap. Today that is exact, because FILE-OF (src/compiler/native/regalloc.f:243-247) is a bijection on the two classes: C-GPR->F-GPR, C-FPR->F-FPR. A64RAV has NO FILE-OF equivalent at all; its file reader is literally FPR? (regalloc-verify.f:2235-2243).

The A64 V registers ALIAS the D registers - v3 and d3 are one register - so a vector class must map to F-FPR. The moment it does, OVERLAP-CK stops checking a C-VEC value against a C-FPR value and two live values can be handed the same physical register with no diagnostic. That is silent wrong code, produced by the validator that exists to prevent exactly it.

Fix: give A64RAV a FILE-OF and key OVERLAP-CK (and REGGED?/FLOATING?/GPR-WRITTEN/FPR-WRITTEN) on the FILE. Regression test: two values of two classes on one file, same register, overlapping live ranges -> E-A64RAV-OVERLAP. Prove the test fails before the fix.

Also in scope, same closed-world defect in the allocator (all verified, with lines): the two SILENT fall-throughs POOL-BITS regalloc.f:346-347 and CALL-BITS regalloc.f:506-509 hand a third class the GPR pool/clobber mask with no throw; and the five literal C-GPR/C-FPR enumerations at regalloc.f:390-391 (TABLES-CLEAR), 1504-1505 (MB-EXPIRE), 1716/1718 and 1724/1726 (MB-STEP pressure), 1732-1733 (MB-SCAN). FILES-N exists (regalloc.f:239) but NOTHING loops over it. The leaf words (MB-FREE-N, MB-READ/WRITE-PRESSURE, MB-LOAD-N, MB-FORBID, MB-PIN, MB-PLACE1) are already class-parameterised, so the repair is at the callers.

Found by agent neon while scoping habu-vectorize-the-byte-a0da35a7.
