---
title: "PTX M2: parametric-type checker extension"
status: open
priority: 1
issue-type: task
created-at: "2026-06-26T23:57:38.088729+02:00"
---

EPIC (large). docs/ptx-sketch.md Milestones #2 + ptx.md Foundational-prerequisites #2. The parametric-type machinery the whole tile DSL needs; M4 (habu-ptx-m4-tile, dot text "also needs M2") and habu-ptx-local-type are BLOCKED-BY this. Owner file: src/core/checker.f (+ src/core/render.f, src/core/check-hook.f - see LESSONS.md "Checker model cutovers must rebind the hook"). Staged self-host bootstrap per Resolved-M1/M2 #5: encode term/unify machinery in old syntax first, refresh the native binary, then accept the parametric syntax. Sub-dots M2a-M2f below. Gate via the exact owning bin/hb --load checker path + a self-host fixpoint rebuild. Strictly typed Habu; new type tokens need a checker-only bootstrap stage (docs/forth.md).
