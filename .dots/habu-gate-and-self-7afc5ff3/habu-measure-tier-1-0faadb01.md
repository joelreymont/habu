---
title: Measure tier-1 latency on the ThinkPad
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:15:14.875927+03:00"
blocks:
  - habu-run-bin-hb-6378f297
---

Problem: the x86 engine is tier-1-only and its gate wall time on the ThinkPad is unmeasured; G4a gives the spark numbers.
Acceptance: the same five heavy suites as G4a timed with and without a `1 set-tier` prefix on the ThinkPad with the cross-built engine; recorded in `docs/compiler-measurements.md` beside G4a's numbers; opens the B2 follow-on dot (tier-1-only products on both arches) with both hosts' numbers. If the cost is not acceptable the answer is tier-1 compile-time work and B2, never a tier-0 port.
Files: `docs/compiler-measurements.md`, `.dots/` (the B2 dot).
Verify: ThinkPad: the five suites timed at both tiers.
Depends: habu-run-bin-hb-6378f297 (X6).
Route: Alder (shared: docs/compiler-measurements.md).
Ownership: krait (Intel lane).
Claim: unassigned.
