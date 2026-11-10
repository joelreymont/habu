---
title: "One reader for a fetch descriptor's cells"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T11:07:49.312352+02:00"
---

Problem: src/habu/t0.f DESC@, PLAIN? and TAG-INLINE read a fact descriptor's cells directly (row count at cell 0; a row's offset, limit and guard count at cells 1 to 3), a layout src/core/layout-valid.f writes (:171, :274-276) and src/compiler/native/fetch-check.f already reads (SLOT@, RUN-ROW, SHAPE). The row layout is spelled in two packages, and DESC@ repeats SLOT@. Found by the review of the inline tag check, whose brief kept fetch-check.f otherwise unchanged. Acceptance: NFETCH-CHECK has public words that answer a descriptor's row count and a row's offset, limit and guard count; t0.f calls them and DESC@ is gone; the cell numbers appear only in fetch-check.f and the writer. Files: src/compiler/native/fetch-check.f, src/habu/t0.f. Verify: the engine built by the new hb equals the engine it builds; test/program-diagnostics-test.f. Depends: habu-inline-t0-s-984cc1e2 landed. Ownership: src/compiler/native/fetch-check.f, src/habu/t0.f (the TAG-INLINE words). Claim: unassigned.
