---
title: Close generic slot protocol split
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.164364+02:00\""
closed-at: "2026-04-03T15:24:40.800672+02:00"
close-reason: done (zig build; zig build test back to known 5-error baseline)
blocks:
  - habu-fix-canonical-structure-cc30a61d
---

Problem: slot-value and related APIs still blur structures with true slot-bearing standard objects or conditions. Acceptance: generic slot protocol rejects non-slot objects and structures only use representation-correct access. Files: src/runtime/primitives/clos.zig, lib/stdlib.habu. Verify: slot-value regressions on structures, standard objects, and conditions. Blockers: habu-fix-canonical-structure-cc30a61d.
