---
title: Derive the catch and match table ceilings anew
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T07:53:40.233271+02:00"
---

The recorder landing (2f988d14) removed the 512-byte wall that made CWIN-MAX 16 and MWIN-MAX 24 unreachable by real code: lib/array-test.f AT-TEST-CHECKS writes 17 catch sites in one definition and now refuses at the table (fail-closed, by name, not a miscompile); a64ir.f OPCODE dispatches over 76 arms against MWIN-MAX 24. Both ceilings need re-derivation from the NEW recorder bounds (BODYBUF-CAP 8000 / source-div-2 tape) with two-sided reachability fixtures per the standing discipline - the derivation requirement is written into the checker prose at both constants. Files: src/core/checker.f, test/compiler/native-catch.f, native-match.f. Depends: none.
