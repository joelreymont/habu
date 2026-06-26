---
title: "PTX AD: device-run finite-difference gradcheck harness"
status: open
priority: 2
issue-type: task
created-at: "2026-06-26T23:59:59.028074+02:00"
blocks:
  - habu-ptx-m6-collectives-12cf0e2d
---

The HARD GATE that makes verified-gradients a real claim. Review crux: the checker proves TYPES not the DERIVATIVE; a wrong VJP entry or wrong algebraic rewrite type-checks and ships a silently wrong gradient. Build a device-run central-difference gradcheck (vs the analytic VJP, per-element relative tol, randomized inputs) usable as a gate over every VJP: entry and every generated backward.
- Files: new tools/ptx/gradcheck.f (Habu-native harness); uses the M1d CUDA Driver harness to launch fwd+bwd kernels.
- Verify: a deliberately-wrong VJP (e.g. the OVER-as-permutation bug) FAILS gradcheck; a correct one passes. Tie-input and saturated-logit fixtures included.
- Dep: M6 + M1d (needs device). Gates every ad-reverse VJP entry.
