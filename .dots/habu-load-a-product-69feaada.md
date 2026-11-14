---
title: Load a PRODUCT of 256 fields
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T18:16:28.857830+01:00"
---

Problem: a word's input row is capped at 255 cells because its minimum input depth is kept in 8 bits of its dictionary record (src/habu/layout.f:132-134 DNAME-MIN-IN; src/core/checker.f:10576 EFFECT-MIN-IN-MAX). A PRODUCT or STRUCTURE of 256 one-cell fields is rejected with "declaration failed at '<name>'", a SUMTYPE variant of 256 cells exits 70 with no message, and DERIVE eq on a 128-field PRODUCT kills the process (rc 76, "checker: min-in exceeds record field"). Only the interpret loop's GATE (src/habu/interpreter.f:497-502) acts on the field, and the top-level check rejects an underdepth call before it runs. Acceptance: GATE's depth test, the field and its writers, and the 255-cell caps are gone; a PRODUCT of 256 fields loads and answers its fields, as a type-decl test; every underdepth case of test/underdepth-gate.f is still rejected before the word runs. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-p256.
