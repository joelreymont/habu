---
title: "Committed autotune: menu, prune, stopwatch, winners"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T14:24:06.440579+02:00\""
---

Parity-plan phase 4 - Habu's answer to @triton.autotune, each mechanic upgraded to Habu discipline. (1) MENU: formalize the tile config as a declared record over the existing knobs (warps, MFRAGS, BK, pad, stages, dyn, epilogue, dtype, B-feed, and round-8 BN, phase-3 split-K) - the candidate space is data, not decorator args. (2) PRUNE: already stronger than Triton's heuristics - the emitter's fail-closed legality guards (E-MMA-SMEM/WARPS/EPI/DTYPE/BTF16) ARE the prune; enumerating the menu against them yields the legal set mechanically, per device caps read at probe time. (3) STOPWATCH: the GB sweep harness under gate discipline - element-exact precondition, solo GPU, sustained-clock verification, best-of-3 - strictly better than Triton's noisy few-rep warmup. (4) WINNERS: committed and reviewed, not silently cached - a winners table keyed (shape-class, dtype) checked into the repo next to perf-rows.tsv, selected by the planner at PLAN time (MODEL:/equation lowering reads it), so first-call runtime autotune stalls do not exist in Habu - deterministic cold start is a headline feature against Triton, not just parity. Unknown shapes bucket to the nearest committed class; a genuinely new class triggers an OFFLINE sweep + review, never a silent runtime bench. (5) DRIFT: a scheduled perf slice re-validates committed winners on spark; regression is a loud gate finding. Depends on rounds 8 and phase 2/3 filling the menu; the table format + selector can land earlier against today's family.

Claim: agent=autotune workspace=.jj-ws/autotune machine=spark
