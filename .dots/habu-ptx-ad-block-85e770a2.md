---
title: "PTX AD: BLOCK-MAX arg-max select + deterministic tie-break"
status: open
priority: 2
issue-type: task
created-at: "2026-06-26T23:59:59.020006+02:00"
blocks:
  - habu-ptx-m6-collectives-12cf0e2d
---

The one GENUINELY new primitive the AD layer needs (autograd.md). A masked scatter: routes ds to the arg-max lane, 0 elsewhere (sub-gradient of BLOCK-MAX). REQUIRES a deterministic tie-break: forward BLOCK-MAX computes only the max VALUE via shfl.sync and does NOT pin an index, and warp reduction order is not stable. Contract: lowest global lane index wins (decided, see autograd.md + PLAN); lower the FORWARD argmax to match, route the ENTIRE ds to that single lane in backward.
- Files: src/arch/ptx/ (the BLOCK-MAX lowering + the new select op).
- Verify: tie-input gradcheck fixture (two lanes equal max) asserts sum(dx)=ds AND the chosen lane equals the forward selection. Single-max fixture routes ds to that lane.
- Dep: M6. Consumed by ad-reverse VJP: BLOCK-MAX.
