---
title: Combine instructions the way ARM64 rewards
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.387176+02:00"
---

A post-selection machine-combine pass over the emitted stream shapes: mul+add/sub to madd/msub; adjacent same-base loads/stores to ldp/stp (directly valuable for remaining data-stack traffic — pairs are exactly what call sites and prologues emit); shift-and-mask to ubfx/sbfx; compare chains under && to ccmp; add/sub with shifted operand folding. Each combine is a peephole with its Rocq-modeled encoder (madd/msub/ldp/stp/ubfx/ccmp need model rows per the Csel precedent), a structural test that the pattern fires (instruction counts via NWALK), and answer pinning. Acceptance: measured against the clang column; the ldp/stp count on call-heavy rows rises to match what clang emits for equivalent frames.

Blocked by: habu-epic-hard-cut-a684f24d phases 1-6. Re-scoped: add only patterns proven useful by emitted inventories (madd/msub, paired same-base memory ops, shifted operands, bitfield extracts, ccmp); validate rewrites against instruction semantics, not raw bytes after publication.
