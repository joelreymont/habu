---
title: Turn deliberate cast sites into declared casts
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:57:00.773557+03:00"
---

Problem: 99 TRUSTED: sites under test/ state an effect their body does not have on purpose (a body `;` declared ( n -- ptr u8 ), a body `0` declared ( -- fresh-region-a-a ), a body `n` declared ( -- [ n -- n ] )): they are unchecked casts, not helpers, and a sweep cannot convert them (sweep lane, 2026-09-16). The tree has a declared cast form (CAST: in lib/string.f, docs/forth.md), audited by name. Acceptance: every cast site becomes a CAST: declaration (or the CAST: form is extended to the shapes the tests need, with the checker rule stated), each with a one-line reason; sites that turn out to be lies about a working word become checked definitions; per-suite case counts unchanged; the count of unchecked casts is reported per file. Files: the test files the sweep lane listed, src/core/checker.f if CAST: needs a shape, docs/forth.md. Verify: the affected suites; test/run.f. Depends: none. Ownership: test tree. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.
