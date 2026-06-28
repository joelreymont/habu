---
title: Structure checker records
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T19:00:38.177623+02:00"
---

Problem: src/core/checker.f now stores canonical effect records but still declares record layouts as manual offsets/parallel cells instead of Habu structure definitions. That keeps layout contracts implicit and easy to corrupt. Fix: use the repo structure DSL for checker-owned records where it fits: effect record header, effect nodes, symbol rows, primitive rows, no-return/control/defer rows. Keep only raw arena byte-copy code at the allocation boundary. Acceptance: record field accessors are generated from structures or thin wrappers over them; raw offset arithmetic is removed from normal lookup/apply code; rebuild bin/hb; engine-suite and full native gate pass; no performance regression in checker-heavy gate slice beyond measured noise.
