---
title: Measure native compiler baseline
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:49.545258+02:00"
closed-at: "2026-08-15T14:07:48.008403+02:00"
close-reason: "Closed SUPERSEDED (vintage audit 2026-08-15, re-executed): native baseline - the judge board is stronger and STANDING (three-column bytes over one text, one-command regen, checked; refusals per-row; costs printed-not-gated satisfying this leaf's own clause; pinned chain baseline; fuzz oracle). Residue recorded: dynamic instructions/stack traffic/compiler size/peak memory never measured - demand-driven per the no-ledgers rule."
blocks:
  - habu-add-pinned-engine-90090800
---

Full context: the existing pinned-engine dot owns checker/fixpoint/interpreter timing, not design section 16.3 native compiler metrics. Add individually pinned JIT/AOT latency, emitted bytes, dynamic instructions, stack traffic, spills, direct/indirect calls, branches, runtime, compiler size, and peak temporary memory for a representative corpus. Acceptance: each metric records machine, target, toolchain, source digest, protocol, and calibrated mutation evidence; no aggregate total is a gate.
