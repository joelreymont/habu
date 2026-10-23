---
title: Ablate tier-1 passes on real workloads
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T18:58:18.974188+03:00"
closed-at: "2026-10-23T20:04:00.846024+03:00"
close-reason: measured; results recorded above
---

Build engine variants (never landed) each with one tier-1 pass off - the loop fold, whole-word data-stack residency (select.f, flushing at joins as a single pass would), copy coalescing - and time the workloads of 'Time real workloads at both tiers' on each. The run-time cost of each pass on real code is what a single pass would lose. The census found 0 loop folds in every corpus, so the fold variant is settled without a run. Lane: jerry, worker in .jj-ws.noindex/jerry-ablate; results ~/.cache/tmp/jerry-ablate/. Claim: jerry.

Result (summary, times, census in ~/.cache/tmp/jerry-ablate/; variant WIP changes zsymkuln = A, xnosruzm = B, never land). Tier-1 run time (whole minus load), median of 3 interleaved reps, wall seconds, ratio to base:

- zz: base 0.95; A 0.94 (0.99); B 0.93 (0.98)
- dots ls: base 5.17; A 5.22 (1.01); B 5.18 (1.00)
- dots tree: base 9.61; A 8.01 (0.83); B 7.97 (0.83)
- tier-bench: base 0.21; A 0.22 (1.05); B 0.22 (1.05)
- tender: base 38.01; A 36.58 (0.96); B 37.75 (0.99); base alone spans 36.0-42.0 s user

Neither variant slows a real workload beyond noise; stdout matches base on every row. B (MB-COALESCE-OP drops its operation, regalloc.f:1424-1426) slows tier-bench's own kernels (arith +48%, branch +26%) and multiplies register moves 7-12x, +2-3% code. A is not the briefed flush: tier 1 has no stack-to-memory traffic at joins to switch on (join block args carry no slot), so A drops the residency fixpoint's cross-join slot knowledge instead (select.f:3007-3008, with the matching verifier refusal lifted at regalloc-verify.f:1771-1772): +0.8-2.8% code, no run-time effect. Engine words are compiled by the base compiler in every variant; only code compiled after `1 set-tier` reflects a variant.
