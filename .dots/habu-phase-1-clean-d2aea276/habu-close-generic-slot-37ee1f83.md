---
title: Close generic slot protocol split
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.164364+02:00"
blocks:
  - habu-fix-canonical-structure-cc30a61d
---

Problem: slot-value and related APIs still blur structures with true slot-bearing standard objects or conditions. Acceptance: generic slot protocol rejects non-slot objects and structures only use representation-correct access. Files: src/runtime/primitives/clos.zig, lib/stdlib.habu. Verify: slot-value regressions on structures, standard objects, and conditions. Blockers: habu-fix-canonical-structure-cc30a61d.
