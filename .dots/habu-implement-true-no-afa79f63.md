---
title: Implement true no-checker PTX ablation
status: closed
priority: 2
issue-type: task
created-at: "2026-06-27T15:35:25.407610+02:00"
closed-at: "2026-07-01T17:53:54+02:00"
close-reason: "completed: maki/eval-device.f adds GRADE-NOCHECK-CANDIDATE with a throwaway 0 set-check driver; maki/eval-compare.f scores every candidate through checked and no-checker arms; Orin proof shows checked catches 5/6 bugs before GPU while no-checker catches 0/6 before execution, emits+assembles all 9, and all 6 bugs fail only as device-wrong"
---

Deep-review finding 2026-06-27: maki/eval-compare.f describes a without-checker ablation, but CMP-SCORE calls GRADE-CANDIDATE and therefore still short-circuits checker-rejected candidates instead of running every candidate through an unchecked emit/ptxas/device path. Correct fix: add a separate no-checker grading path that emits/assembles/runs every candidate, counts runtime/device failures distinctly, and updates maki/README.md/docs/eval-triton.md only after measured evidence. Verify with candidates that the checker rejects but the no-checker path still attempts to run or fails later in the intended class.
