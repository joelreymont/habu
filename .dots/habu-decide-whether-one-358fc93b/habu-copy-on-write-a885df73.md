---
title: Keep tier 0 locals in registers
status: active
priority: 3
issue-type: task
created-at: "2026-10-26T11:18:06.207865+02:00"
---

Scheduled by Joel 2026-10-27 ("dot and implement then"), part of carrying tier 1's register use into tier 0 before tier 1 is deleted. Was deferred by Joel 2026-10-26: tier 0 join allocation is T0-04 minus aliases (join layouts at if, parallel-copy reconcile at else/then, selective spills). This dot holds the excluded part: let a local and a stack value share one register until either is written, instead of reloading the local from its frame slot. Not scheduled: tier 1 has no such aliasing (it also reloads locals from frame slots) and the zz classification shows no tier 1 gain in allocation/spills/locals (f = -45 of 2598 samples, habu-classify-tier-1-df654e77). Take it up only if a measured tier 0 residual after the join allocator is attributed to local reloads.

Problem: C-LBRACE-BIND-VS (src/habu/habu2.f ~6586) stores every bound value to a frame slot, and C-LOCAL-REF (habu2.f ~6932) emits a fresh ldr into a new pool register on every read; nothing is cached, and the binding value's register is freed. Acceptance: between a local's binding (or its last write) and the next call, join or spill, a read reuses the register already holding the value instead of loading; writes keep the slot and any cached copy coherent; behaviour at every call, join, spill, `exit`, throw and nested locals group is unchanged. Tests: the tier0-inline-prims harness shows a repeated local read emits no ldr; runtime-regression rows for writes, joins and calls. Measure zz and tender run instructions before/after. Gate: test/compile-floor-gate.f. Files: habu2.f (C-LBRACE-BIND-VS, C-LOCAL-REF, local writes), jit.f/regalloc.f as the design needs. Depends: none. Ownership: tier 0 locals codegen. Claim: unassigned.
