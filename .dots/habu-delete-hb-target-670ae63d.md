---
title: Delete HB-TARGET-KNOWN?, which nothing calls
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T22:32:33.666159+02:00"
---

Problem: after src/compiler/native/compiler.f lost its dead target check, HB-TARGET-KNOWN? (each src/os/*/target.f:18-20) has no caller in src, tools, Zen, Tender or Maki; only test/engine-suite.f TTARGET-KNOWN (:177-178) calls it, and src/habu/verify-source.f:968, tools/reserved-name-lint-core.f:215 and docs/forth-card.md:88-89 list its name. Every seam answers true, so it states nothing. Acceptance: the word, its test use and its three listings are gone; the chain build reaches its fixpoint and the suite passes. Depends: none. Claim: unassigned.
