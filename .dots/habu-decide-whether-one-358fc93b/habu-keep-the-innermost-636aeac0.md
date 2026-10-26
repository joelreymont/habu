---
title: Keep the innermost ?do counters in registers
status: active
priority: 3
issue-type: task
created-at: "2026-10-26T11:28:18.271846+02:00"
---

Experiment step 3, last (Joel 2026-10-26): 0% of zz's gap, 5% of tender's. Cache the innermost ?do index in a register; the memory frame stays home (J-I habu2.f ~2821 is 8 instructions). Then measure the real workloads and rule on the 5% bar.

Claim: jerry. Design ~/.cache/tmp/jerry-idx-design.md (Plan/Fable, lead cuts in section 7): x29 leaves the integer pool (16 -> 15) and caches the innermost index; a compile-time flag LIDX-CELL is cleared at every CF push/pop, emitted call, recurse, unloop and rollback; `i` becomes a non-spilling row. Workspace .jj-ws.noindex/jerry-idx; the worker starts after the joins+widening measurement finishes.
