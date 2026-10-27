---
title: Keep the innermost ?do counters in registers
status: active
priority: 3
issue-type: task
created-at: "2026-10-26T11:28:18.271846+02:00"
---

Experiment step 3, last (Joel 2026-10-26): 0% of zz's gap, 5% of tender's. Cache the innermost ?do index in a register; the memory frame stays home (J-I habu2.f ~2821 is 8 instructions). Then measure the real workloads and rule on the 5% bar.

Claim: jerry. Design ~/.cache/tmp/jerry-idx-design.md (Plan/Fable, lead cuts in section 7): x29 leaves the integer pool (16 -> 15) and caches the innermost index; a compile-time flag LIDX-CELL is cleared at every CF push/pop, emitted call, recurse, unloop and rollback; `i` becomes a non-spilling row. Workspace .jj-ws.noindex/jerry-idx; the worker starts after the joins+widening measurement finishes.

Landed 107761fb "Keep the innermost tier 0 loop index in x29" (Fable ACCEPT; its one note, the redundant `i i *` row GEISQ, was dropped). Gate (~/.cache/tmp/jerry-gateidx/): full suite red only lsp-boundary, check-drivers, check-cli-boundary at load 41, all pass alone (alone/); rebased onto 26408387: two-gen converged, tier0-join, tier0-compare-branch, tier0-inline-prims, runtime-regression, tier, compile-floor-gate rc 0. Rollback row not added: GE-FAMILY needs no open loop, and per-token marks keep the `i` reload compiled, so the row could not catch a missing clear. Measurement against jerry-widen: brief ~/.cache/tmp/jerry-imeasure-brief.md, results ~/.cache/tmp/jerry-imeasure/.
