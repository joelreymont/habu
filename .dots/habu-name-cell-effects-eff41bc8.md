---
title: Name cell-effects.f for what it holds
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T16:20:04.391201+01:00"
---

Problem: src/core/cell-effects.f no longer holds any effect row: it defines the top-level row words parses:, parses-through: and names: and the CHECKER-CALLS call-window store, so its name misleads. Acceptance: the file (or the two files its two subjects become) is named for its contents; src/habu/boot-files.f, src/habu/xref.f and the open dots that cite it follow; the engine converges. Depends: none. Claim: unassigned.
