---
title: "Infer prefill: supported geometry"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.721976+02:00"
blocks:
  - habu-infer-dense-tensor-c037a6fd
---

Why this exists:
fast prefill needs a bounded shape, dtype, masking, and context contract for the pinned model before kernel work.

Required result:
define checked prefill geometry and workspace derivation for the supported prompt regimes.

Done when:
1K, 4K, 16K, and declared longer rows pass; invalid rank, head mapping, mask, workspace, and overflow reject before launch.

Expected touch points: new lib/ptx/prefill-geometry.f, focused test.
Smallest check: focused geometry test.
Prerequisites: pinned modern-model geometry.
Owned result: prefill geometry only.
Claim: unassigned.
