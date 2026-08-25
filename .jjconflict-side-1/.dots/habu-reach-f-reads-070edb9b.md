---
title: reach.f reads TERM counts where the chain moves CELLS
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T12:59:04.162331+02:00"
---

Problem: src/compiler/native/reach.f:122 wraps the checker's effect-read export as `TRUSTED: EFFECT ( ptr u8 n -- n n ) EFFECT-QUERY if EFFECT-DIN-N EFFECT-DOUT-N else -1 -1 then ;` - the same terms-not-cells shape dot habu-export-the-checker-2bbc831c removed from src/compiler/native/dict.f and tools/chain-census-core.f. A term is not a cell: a signature carrying one term of a multi-cell layout family reports one where the call site moves W. Every other consumer of that export now reads EFFECT-DIN-CELLS / EFFECT-DOUT-CELLS, published by the checker from its own ROW-TERM-CELLS. Acceptance: reach.f asks for cells, or its header states why a TERM count is the right number for what reach.f does with it (it may genuinely want terms - that has not been established, and the file's comment does not say). Files: src/compiler/native/reach.f. Verify: bin/hb --load test/run.f; the reach slice. Depends: habu-export-the-checker-2bbc831c (landed the widths). Ownership: reach surface. Claim: unassigned.
