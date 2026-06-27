---
title: "AD: thread saved values to real buffers (resolve SAVED-*)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T08:06:44.237997+02:00"
blocks:
  - habu-ptx-ad-reverse-26aebee3
---

Gap #6. The reverse pass emits SAVED-Y/SAVED-X as PLACEHOLDER tokens; they are not resolved to actual saved primal/output buffers. Implement the save-vs-recompute LOWERING: per the policy (VJP-SAVES + AD-RECOMPUTE?), either stash the forward value to global and reload in the backward, or recompute the forward slice. Replace SAVED-* placeholders with real loads/recompute in the generated backward.
- Files: lib/ptx/ad.f (resolve SAVED-* during emission), the codegen.
- Verify: a generated backward with EXP. (saves y) loads/recomputes y correctly; gradcheck on device.
- Dep: ad-reverse + the tile-IR codegen + the save-vs-recompute policy (built).
