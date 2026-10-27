---
title: Keep the innermost ?do counters in registers
status: closed
priority: 3
issue-type: task
created-at: "2026-10-26T11:28:18.271846+02:00"
closed-at: "2026-10-27T03:40:59.669559+02:00"
close-reason: "landed 107761fb; measured: no real-workload change"
---

Experiment step 3, last (Joel 2026-10-26): 0% of zz's gap, 5% of tender's. Cache the innermost ?do index in a register; the memory frame stays home (J-I habu2.f ~2821 is 8 instructions). Then measure the real workloads and rule on the 5% bar.

Claim: jerry. Design ~/.cache/tmp/jerry-idx-design.md (Plan/Fable, lead cuts in section 7): x29 leaves the integer pool (16 -> 15) and caches the innermost index; a compile-time flag LIDX-CELL is cleared at every CF push/pop, emitted call, recurse, unloop and rollback; `i` becomes a non-spilling row. Workspace .jj-ws.noindex/jerry-idx; the worker starts after the joins+widening measurement finishes.

Landed 107761fb "Keep the innermost tier 0 loop index in x29" (Fable ACCEPT; its one note, the redundant `i i *` row GEISQ, was dropped). Gate (~/.cache/tmp/jerry-gateidx/): full suite red only lsp-boundary, check-drivers, check-cli-boundary at load 41, all pass alone (alone/); rebased onto 26408387: two-gen converged, tier0-join, tier0-compare-branch, tier0-inline-prims, runtime-regression, tier, compile-floor-gate rc 0. Rollback row not added: GE-FAMILY needs no open loop, and per-token marks keep the `i` reload compiled, so the row could not catch a missing clear. Measurement against jerry-widen: brief ~/.cache/tmp/jerry-imeasure-brief.md, results ~/.cache/tmp/jerry-imeasure/.

Result (worker over jerry-widen 513d0aa7 vs jerry-idx 107761fb; user CPU run medians base t0 -> cand t0 (cand t1); 5-7 reps alternating; outputs identical; artifacts ~/.cache/tmp/jerry-imeasure/): tier-bench whole run .32 -> .27 s (.26; 1.04x), kernels 17185 -> 14101 us (5031; -18%, 2.80x; branch 8132 -> 5341); zz 1.24 -> 1.23 (.92; 1.34x); dots ls 1.43 -> 1.43 (1.25; 1.14x); dots tree 3.10 -> 3.11 (2.83; 1.10x); tender c2-office-scale 42.13 -> 42.09 (35.64; 1.18x). Compile floor t0 trivial 36 vs 36 us. Code shape: tender READ-COLUMN 65 -> 56 instructions, LB-NEAREST-LABEL 163 -> 134; `i` is one mov, but real loop bodies call words, so x29 reloads after each call and the index is stored back on every `loop`. Real workloads do not move: the remaining tier 0 gap is not loop-index cost.
