---
title: Mark blob-provided rows in prims.f
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.621343+03:00"
closed-at: "2026-09-29T15:53:51.613543+03:00"
close-reason: "folded into I9a (habu-move-evaluate-and-9119f746): no row moves before it, so a standalone mechanism had no failing check"
---

Problem: `src/habu/prims.f` knows only kernel bodies (`prims.f:19-22`); rows whose body moves to a checked prefix file need a spelling.
Acceptance: a row kind naming the prefix file (prefix-provided rows); `habu1.f FP-ARGS` refuses an assembly body for such a row in a product build (kept for `SEEDED-RUNTIME?`-false builds); the completeness gate accepts it when the file is in the manifest and the name resolves after the prefix loads; `test/prim-parity.f` runs unchanged.
Files: `src/habu/prims.f`, `src/habu/habu1.f`, `src/habu/primitive-registry.f`.
Verify: spark `bin/hb --load test/prim-parity.f`; rebuild; gate.
Depends: habu-share-the-primitive-58c235e5 (K4).
Route: Alder (shared: src/habu/prims.f, src/habu/habu1.f, src/habu/primitive-registry.f).
Ownership: krait (Intel lane).
Claim: unassigned.
