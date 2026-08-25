---
title: "Scout Triton's 5-stage loop structure under 13.3"
status: closed
priority: 2
issue-type: task
created-at: "2026-07-19T14:23:36.368634+02:00"
closed-at: "2026-07-19T15:48:31.394317+02:00"
---

Parity-plan phase 3b, read-only scout BEFORE any ring rework: our N-stage cp.async ring loses at every shape even under the 13.3 assembler (re-measured 2026-07-19), while Triton's 512^3 winner is a 5-stage 4-warp kernel on the same hardware. Something structural differs in how the loops overlap: candidate differences to adjudicate with SASS+control-word evidence - prologue/epilogue drain shape (ours drains wait_group N-2..0 serially), where the barrier sits relative to the wait, whether Triton overlaps the NEXT tile's cp.async issue under the CURRENT tile's mma burst, and per-stage smem slot addressing (ring arithmetic vs unrolled slots). Deliverable: the named structural difference plus an actionable emitter-change verdict with predicted effect, or a proof our ring is equivalent and the loss is elsewhere. Compile+dump only.
