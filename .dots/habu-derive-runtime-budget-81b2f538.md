---
title: Derive macOS runtime-slice budget for loaded hosts
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T08:45:00+02:00"
---

The native gate's "runtime slice" time ratchet (max-ms=10000) is derived from a quiet machine, but the repo's normal operating mode is now several orchestrated worker lanes running fixpoint rebuilds and gate batteries concurrently on the same host. Measured evidence 2026-07-19 on the merge-gate tree (engine e05851ef, byte-identical to the fixpoint that passed the same ratchet quiet 30 minutes earlier): elapsed 11504 ms at load ~6.8 with three lanes building, and 10986 ms at load ~5.8 with lanes idling - both red on a tree whose only delta was dot text and LESSONS.md, i.e. pure load contention, not an engine regression. The spark host class already solved this correctly (commit 9d91057e "Give spark/macOS cold gate its own perf budget" and 76f5e652 "Re-derive spark cold budgets on healed cache" re-derive budgets from measured cold runs per host class). Do the same derivation for this slice's macOS budget: measure a documented cold baseline, state the multiplier, and either derive the budget from that measurement or make the ratchet load-aware (e.g. record and report host load with the measurement so a red under saturation is attributable). Do NOT just bump the constant - the ratchet must still catch a genuinely slower engine. Files: the runtime-slice budget constant in the gate (rg max-ms test/gate-*), docs if the derivation is recorded there. Verify: quiet run passes with margin; a deliberately loaded run either passes within the derived budget or fails with a load-attributed diagnostic; the ratchet still fails on an artificially slowed slice.
