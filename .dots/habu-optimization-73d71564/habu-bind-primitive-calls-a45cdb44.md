---
title: Bind primitive calls before image emission
status: open
priority: 1
issue-type: task
created-at: "2026-09-24T17:18:42.984066+02:00"
---

The current Mac engine carries 9,638 primitive-call rows (115,656 bytes). Every stored BL has zero displacement, every scope is zero, and targets are seeded primitives. EM-SEED-AOT copies the blob into the runtime JIT mapping and resolves these rows at boot; the original table has no later semantic consumer, while site provenance remains necessary for snapshots.

Bind complete-engine calls against a canonical text/region distance in the writer, then adjust by the actual runtime distance and preserve the call map. Existing SNAP-RELOC:EMIT-CALLS already performs distance adjustment. Four-byte site offsets would cost 38,552 bytes: 77,104 bytes gross potential reduction before code, framing and alignment. Preserve generic named relocation for partial captures. Do not assume the kernel grants the mapping hint.

Own src/habu/habu2.f emission/seeding and the affected capture/reader format. Verify primitive targets and branch bounds, fresh starts at different region distances, native self-build fixpoint, partial capture, stripped builds and REPL snapshot/recapture. Measure actual section and file deltas; rebuild and run test/run.f plus the Maki readiness smoke before landing. No dependency on dictionary or checker pruning. Evidence: the Optimization parent baseline and temporary habu-reloc-rca.f probe.
