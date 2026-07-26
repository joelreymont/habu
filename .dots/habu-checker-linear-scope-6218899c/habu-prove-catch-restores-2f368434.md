---
title: Prove catch restores linear owners
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:48:57.220131+02:00"
blocks:
  - habu-unify-all-quotation-56884608
---

Problem: RSCATCH unifies only a quotation normal output with its input. If a throw path consumed, disposed, or deconstructed a linear value, catch still restores the old stack cells and the checker certifies a forged live owner. Required result: when a caught quotation can throw and its entry data or return row contains a linear value, RSCATCH must prove the recorded exceptional rows equal the quotation entry rows with the same nominal linear identities and multiplicities. A mismatch rejects at catch; non-linear catch behavior and a quotation with no throw remain unchanged. Do not add runtime guards or special-case MATCH by spelling. Owner: catch linear-restoration rule in src/core/checker.f. Dependency: habu-unify-all-quotation-56884608. Acceptance: negative checked regressions reject owner-drop-then-throw, dispose-then-throw, return-stack abandonment, and MATCH payload consume-then-throw; a stack-preserving body throw accepts; branch order cannot change verdict. Use the real CHECK!/bin/hb path and existing type-linear and type-match providers.
