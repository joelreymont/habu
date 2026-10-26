---
title: Copy-on-write aliases for tier 0 locals
status: open
priority: 3
issue-type: task
created-at: "2026-10-26T11:18:06.207865+02:00"
---

Deferred by Joel 2026-10-26: tier 0 join allocation is T0-04 minus aliases (join layouts at if, parallel-copy reconcile at else/then, selective spills). This dot holds the excluded part: let a local and a stack value share one register until either is written, instead of reloading the local from its frame slot. Not scheduled: tier 1 has no such aliasing (it also reloads locals from frame slots) and the zz classification shows no tier 1 gain in allocation/spills/locals (f = -45 of 2598 samples, habu-classify-tier-1-df654e77). Take it up only if a measured tier 0 residual after the join allocator is attributed to local reloads.
