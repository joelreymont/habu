---
title: "typed-top: checker top-row tracker (tier 1, warnings only)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T10:38:59.760957+02:00"
---

Sub-dot 3 of docs/typed-top-level.md sec 5 (landed 8cefda08). Files: new src/core/top-row.f (row state, literal typing, E-INST unify, gray/dirty reseed, 0 set-check suspension, depth anchor, warning renderer), prelude install, test/top-row-warn-test.f. Acceptance: p1/p2/p3 probe shapes each produce exactly one named warning rc 0; eval-fixture idiom + CHECK! probe + mid-stream TRUSTED: shim + 0 set-check window produce ZERO warnings; row persists across require; depth-anchor self-check clean over test/run.f. Depends: hook sub-dot.
