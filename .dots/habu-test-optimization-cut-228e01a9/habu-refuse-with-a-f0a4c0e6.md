---
title: Refuse ; with a quotation open on the JIT tier
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T00:07:43.841092+02:00"
---

Problem: with the checker off, `;` reached while a `[:` is open publishes a body that hangs when run. /Users/joel/.cache/tmp/kestrel-r4/dots/b3.f (`TRUSTED: X ( -- ) [: 1 ;` then `X .s cr`) and /Users/joel/.cache/tmp/kestrel-r4/dots/b2.f (two levels open) load rc 0, and calling X never returns (timeout 20 s, rc 124) on the round-4 head engine. A checked definition cannot reach this (the checker refuses the unclosed quotation); only TRUSTED: and checker-off loads do. Its mirror, `;]` with no `[:` open, is already refused by name. Acceptance: `;` with any quotation open (QPATCH-CELL set or JIT-QUOT depth > 0) refuses by name before the definition is published, rc 75 like the quotation-nesting-full refusal and catchable inside evaluate; b2/b3 rows fail first on the parent engine and pass after; the depth and Q cells are reset so the next definition compiles. Files: src/habu/habu2.f (J-SEMIQUOT / the `;` path), src/habu/layout.f JIT-QUOT, test/runtime-regression-test.f. Verify: rebuild, g1 = g2 with .names, focused tests (test/runtime-regression-test.f, test/native-quot-scope.f).
