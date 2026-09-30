---
title: Keep live using ABI stable
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-30T09:52:59.874982+02:00\""
---

PD-CAP growth from48 to64 moved USE-DEPTH from9C08 toA408. The freshly loaded checker reads that slot before target layout loads, while the retained compiler still owns9C08; recovery still mirrors48. Preserve the published using slot9C08 and all later native bands; move private pending storage after UNIT-COMPILE-CELL, mirror capacity and slot shape in recovery after its private bands. Astra source review confirms cold source writer binds the fresh target layout; saved builder is ineligible for this transition. Acceptance before production edits: existing protection span cases must distinguish guarded unit cell from unguarded pending tail and heap; use existing pre-trust49th defer, using behavior, mirrored layout and data-claim checks on a new generated host, then native gate, convergence and required recovery audit. Swift owns isolated rebuild-integrate; preserve prior accepted worker sources and fixed6b993 base. No speed claim.
