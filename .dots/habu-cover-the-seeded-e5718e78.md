---
title: Cover the seeded-row package fold with a prefix-probe tick
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T08:24:47.298627+02:00"
---

Problem: CK-AOT-PKG= (src/core/checker.f, beside CK-AOT-NAME=) folds the probe byte so a seeded row's folded package matches a token spelled as written, but no row reaches that fold with an unfolded probe: a qualified top-level token on the cold seeded image (test/aot-payload-graph.f, `41 PAYLOAD-NATIVE:BUMP .`) verifies on an engine with the exact comparison too (.out/tj2/apg-q-e2d.log in carl-topjudge), so the engine's own lookup answers it before any row is wanted. The only queue writer that keeps a token as written is IS-TARGET-SYM, inside a prefix probe (CHECKER-PREFIX:BIND?, reached from definers.f DEF-PREFIX-BIND while a definition is pending), serving `is` targets and `[']`. Found by the Opus re-review of habu-refuse-a-top-57851ac2. Acceptance: a row on the cold seeded image of test/aot-payload-graph.f (BUMP has no checker symbol) where a word evaluated during a pending definition ticks `['] PAYLOAD-NATIVE:BUMP` (the package fold) and `['] BUMP` under using (the name fold), spelled upper-case; the definition certifies and runs; the row fails with CHECKER-FOLD-C replaced by an identity in both comparisons and passes as landed. Files: test/aot-payload-graph.f. Verify: bin/hb --load test/aot-payload-graph.f. Depends: habu-refuse-a-top-57851ac2. Ownership: the fixture's completion and control cases. Claim: unassigned.
