---
title: Bind a quotation to a defer through its cell
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T07:44:39.231077+02:00"
---

S2 of the quotations design: is = resolve the defer's record (NDICT:SPELL-REC, XREF-START+LEN), verify DEFER-MAGIC , +8 for the cell, SNAP-RELOC:LMARK it ONCE (the xt-cell declaration - omitting it is exactly the crash of habu-relocate-persisted-defer-7aa681c4, so the snapshot write/restore assertion IS the gate), then hir.store of the code-ref into that constant address. Zero new opcodes; the target name is a meta-operand (MOPERAND? mechanism, elaborate.f:2031). Chain callers of defers already compile (probed: a defer is an engine trampoline, the caller does plain bl - dispatch costs nothing). Acceptance: INIT migrates and ACTION runs IMPL after; xt-cell count moves and a snapshot round-trip leaves the defer working; is on a non-defer refused by name incl. the adversarial record-followed-by-lucky-cell fixture; census is bucket falls from 16. Files: src/compiler/native/elaborate.f. Depends: habu-compile-a-quotation-04341c80.
