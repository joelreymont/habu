---
title: Structure checker records
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T19:00:38.177623+02:00"
---

Problem: src/core/checker.f now stores canonical effect records but still declares record layouts as manual offsets/parallel cells instead of Habu structure definitions. That keeps layout contracts implicit and easy to corrupt. Fix: use the repo structure DSL for checker-owned records where it fits: effect record header, effect nodes, symbol rows, primitive rows, no-return/control/defer rows. Keep only raw arena byte-copy code at the allocation boundary. Acceptance: record field accessors are generated from structures or thin wrappers over them; raw offset arithmetic is removed from normal lookup/apply code; rebuild bin/hb; engine-suite and full native gate pass; no performance regression in checker-heavy gate slice beyond measured noise.

Progress 2026-06-30: converted `EFF-REC` and `EFF-NODE` from manual byte offsets to `BEGIN-STRUCTURE` field accessors in `src/core/checker.f`. To keep bootstrap sound, moved structure definitions before `checker.f`, split checker effect publication into `src/core/structures-effects.f`, and made `DOES>` created-word effect publication skip only while no checker hook is installed. Proof: local Gforth no-binary check-only bootstrap passed in 24.26s; `tools/bootstrap.sh` full local install refreshed `bin/hb` to fixpoint; build-fixpoint-test, bootstrap-codegen-test, vector-test, dictionary/checker phase, trust/filemap/status/host/dot/typed-local lints passed; full native suite passed cold at 45.493s internal / 47.86s wall and hot at 24.383s internal / 26.81s wall. Remaining: symbol rows, primitive rows, no-return/control/defer rows.
