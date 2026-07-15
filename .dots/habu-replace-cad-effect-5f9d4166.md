---
title: Replace CAD-EFFECT pass-throughs with EXPORT aliases
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T18:12:21.366712+02:00"
---

Found by the nomexport lane 2026-07-15 while collapsing effect-row into NOM:row (habu-export-public-nom-20170121): src/cad/effect.f's CAD-EFFECT public surface keeps SIZE / EQUAL? / KEY / ENCODE / UNION as thin pass-through definitions that now merely call the identical NOM:* words on the same NOM:row handles. The bodies add no behavior and duplicate effects. Fix: replace each pass-through with the package EXPORT alias mechanism (docs/forth.md section Packages: EXPORT NAME publishes an existing word under the package tail - one body, two names, checker records an alpha-equivalent scheme) so the CAD-EFFECT surface keeps its names without duplicate bodies; callers unchanged. This is a public-surface shape change - confirm no consumer depends on the words being distinct xts (rg callers; AOT emits one body for aliases). Acceptance: pass-through bodies deleted, EXPORT aliases in place, effect suites + nominal suites green, typed-local-diff clean. Files: src/cad/effect.f, maybe effect tests. Verify: src/cad effect suites x3, lib/nominal suites, maki/test.f. Ownership: cad effect surface.
