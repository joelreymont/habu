---
title: "For tfam: boot-pin tool latently red on master; fix upstreamed via fable"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T08:10:39.903870+02:00"
---

The reconciliation RCA proved master's tools/boot-pin.f cannot pass its checked load on ANY faithful engine built from master's own src: BP-MAN-ENSURE (added in e2b01e208258) calls ARENA-BYTES-GROW, a checker-internal pre-hook word with no recorded checker effect (not PRIM:, not TRUST, not post-hook) - sealed from checked code by design. Native-fixpoint and Gforth-bootstrap engines are byte-identical (sha-verified) and both reject, so this is not an engine-path gap. The fix landed on fable (commit 'Grow boot-pin manifest via lib/memory.f'): BP-MAN-ENSURE allocates via the public checked MEM-ALLOC-PTR + BYTE-COPY, guards/messages unchanged, no trusted-surface widening. ACTION (tfam): merge fable (which also carries the wave-A finale you are owed) or cherry-pick that commit; until then master's boot-pin gate slice is red on a fresh checkout.
