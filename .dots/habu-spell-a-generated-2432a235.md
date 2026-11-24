---
title: Spell a generated word as the load does in the check
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T18:08:38.936158+01:00"
---

Problem: under tools/check.f --verify-only a generated word's name is the symbol store's folded name: for package Q with STRUCTURE sq and a public SQ-MAKE, the load names the private twin SQ-MAKE and the check sq-make; a private sum constructor (SUMTYPE shp VARIANT circ) likewise. The check compiles nothing and a generated registrar keeps no spelling (NAV-PAUSE, src/core/checker.f:10416). Acceptance: for both programs, and for every word a definer generates, the check's record equals the load's byte for byte; a test in test/diag-position-test.f holds both. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-genspell.
