---
title: Retract or re-verify the Orin 1.60x Triton claim
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-19T14:21:12.843093+02:00\""
closed-at: "2026-07-19T15:50:13.056913+02:00"
---

Joel 2026-07-19: the Orin 1.60x-Triton head-to-head measurement was likely a mistake - stop citing it. It appears in docs/eval-triton.md, docs/kernel-principles.md (roofline section), docs/archive/lessons-2026h1.md, and two open dots (habu-feed-mma-config-d783e33b, habu-ship-swizzled-mma-7b78c01b). Task: (1) audit HOW that number was produced (find the original measurement record: which Triton version, which shapes, what clocks, whether the referee kernel was actually comparable - the suspicion is it was not); (2) either re-verify on the Orin with the same referee discipline as the GB10 rounds (source-built Triton, per-shape autotune winner, sustained clocks, best-of-3) or RETRACT: annotate every live-doc citation as 'unverified, likely mistaken (2026-07-19)' until a clean measurement exists. Do not delete history (the archive copy stays as-is); live docs must not carry a decision-grade claim we do not trust. No GB10 conclusion depends on it - the GB10 campaign stands on its own referee runs.
