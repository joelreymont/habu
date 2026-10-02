---
title: "Fit Gforth recovery's stdin text in its buffer"
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T09:24:42.899145+02:00"
---

Problem: Gforth check-only recovery (HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 GFORTH=... tools/bootstrap.sh) passes hb-stage0 and the hb-stage fixpoint, then dies in hb-stdin-mk with 'hb: source prefix buffer full', exit 74. Measured by the r4-aotreq lane (115479b4, dot 9c105898) on its tree and on the parent 53e02ad8 run in full: the baked stdin text is 3,173,951 bytes, the stripped prefix about 1.11 MB, together about 90 KB over the 4 MiB limit; a parent-sized text overflows too, so the lane did not cause it. Master b1d93da8 holds b20c2e72 'Size the source arena from the baked source' and fc5d162b 'Keep boot reads within their source allowance', which may cover it. Acceptance: rerun on the tree after the master merge (r4-mastermerge); if it still dies, the buffer is sized from what it must hold (no new fixed constant picked to fit today's text) or the text shrinks at the responsible layer, and Gforth check-only recovery exits 0. The final gate runs this recovery. Base: after the master merge.
