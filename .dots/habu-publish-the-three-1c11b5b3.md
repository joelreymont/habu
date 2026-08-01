---
title: Publish the three branching corpus words in the comparison table
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T11:06:57.230374+02:00"
---

tools/codegen-compare-new.f still declares MAX2, SUM-TO and COUNT-DOWN gaps waiting on control flow and comparison. When the multi-block allocator, validator and emitter are through, move those three from GAP-CASES to COVERED-CASES with the bodies the chain compiles ('MAX2 2dup < if swap then drop', 'SUM-TO 0 swap 0 ?do i + loop', 'COUNT-DOWN begin 1- dup 0 <= until'), give each enough scratch registers (MAX2 needs four, the loops five or six), execute them on the old column's pinned inputs, and regenerate tools/codegen-compare-baseline.f in the same change with the reason written down. The loops are the rows where codegen quality shows: the old emitter costs 15063 for SUM-TO and 25781 for COUNT-DOWN.
