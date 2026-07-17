---
title: Detect repeated emitted machine-code blocks
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T23:24:18.285035+02:00"
---

Build a checked Habu-native report over the emitted candidate __text that normalizes PC-relative branch/address immediates and reports repeated aligned blocks >=32 bytes with byte totals and owning engine-size regions. This is evidence only: no automatic rewriting and no host-language tooling. Depends on Subdivide mixed engine size regions. Files: tools/engine-clone-report.f plus focused tests and FILEMAP. Acceptance: fixtures distinguish identical bodies, relocation-only clones, overlaps, and near misses; report identifies current duplicate bytes deterministically without allocations in the scan hot path.
