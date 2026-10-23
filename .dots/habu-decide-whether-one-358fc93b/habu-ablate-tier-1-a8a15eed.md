---
title: Ablate tier-1 passes on real workloads
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T18:58:18.974188+03:00"
---

Build engine variants (never landed) each with one tier-1 pass off - the loop fold, whole-word data-stack residency (select.f, flushing at joins as a single pass would), copy coalescing - and time the workloads of 'Time real workloads at both tiers' on each. The run-time cost of each pass on real code is what a single pass would lose. The census found 0 loop folds in every corpus, so the fold variant is settled without a run. Lane: jerry, worker in .jj-ws.noindex/jerry-ablate; results ~/.cache/tmp/jerry-ablate/. Claim: jerry.
