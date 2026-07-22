---
title: "Infer sampler: deterministic device dispatch"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.709819+02:00"
blocks:
  - habu-infer-sampler-bounded-57bfc55a
  - habu-infer-sampler-bounded-c259e11a
---

Why this exists:
mode selection, RNG state, scratch ownership, and device result transfer must advance atomically across all sampler kernels.

Required result:
add typed dispatch keyed by sampling parameters and a generation-bearing seeded state.

Done when:
run twice is identical, each successful call consumes exactly the expected RNG draws, failures do not advance state, and unsupported modes reject.

Expected touch points: new maki/infer/device-sampling.f, focused test.
Smallest check: focused dispatch plus device parity.
Prerequisites: top-p and top-k kernels.
Owned result: device sampling dispatch and RNG transaction only.
Claim: unassigned.
