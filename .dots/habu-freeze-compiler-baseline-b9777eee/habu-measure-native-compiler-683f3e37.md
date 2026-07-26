---
title: Measure native compiler baseline
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:49.545258+02:00"
blocks:
  - habu-add-pinned-engine-90090800
---

Full context: the existing pinned-engine dot owns checker/fixpoint/interpreter timing, not design section 16.3 native compiler metrics. Add individually pinned JIT/AOT latency, emitted bytes, dynamic instructions, stack traffic, spills, direct/indirect calls, branches, runtime, compiler size, and peak temporary memory for a representative corpus. Acceptance: each metric records machine, target, toolchain, source digest, protocol, and calibrated mutation evidence; no aggregate total is a gate.
