---
title: Enforce kernel manifest ABI
status: closed
priority: 1
issue-type: task
created-at: "2026-07-19T20:45:41.401621+02:00"
closed-at: "2026-07-21T21:37:08+02:00"
close-reason: "Superseded: the unbuilt Zig kernel consumer was deleted, so this consumer-only validation work has no remaining code owner. The checked Habu exporter and language-neutral manifest contract remain covered by their existing tests; no replacement consumer work was created."
---

This task described validation work for the former unbuilt Zig demonstration.
That consumer has been removed, so the task is superseded rather than carried
into a replacement host-language example. The checked Habu exporter and the
language-neutral `habu-kernel-manifest` contract remain in place.

Historical finding retained as evidence: `examples/kernel-consumer/main.zig`
claimed every launch argument was derived from and preflighted against the v1
manifest, but it did not validate device and driver capability, address size,
the complete ordered slot layout, launch bounds, or overflow-safe grid
derivation. The task would have required a typed consumer validator and focused
negative coverage if that example had remained an owned repository surface.
