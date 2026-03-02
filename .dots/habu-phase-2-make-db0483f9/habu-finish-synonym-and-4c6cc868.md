---
title: Finish synonym and composite streams
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.217927+02:00"
blocks:
  - habu-unify-stream-and-a262987a
---

Problem: synonym, two-way, echo, and related composite streams do not delegate full query, control, and I/O behavior. Acceptance: symbol-indirected synonym streams and composite stream introspection are correct. Files: src/runtime/primitives/io.zig composite-stream machinery, ../maxima/src/macdes.lisp:28-86. Verify: composite/synonym stream regression set and batch query I/O probes. Blockers: habu-unify-stream-and-a262987a.
