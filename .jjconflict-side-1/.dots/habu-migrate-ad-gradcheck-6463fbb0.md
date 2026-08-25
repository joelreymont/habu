---
title: Migrate ad-gradcheck-launch under its ship contract
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T06:39:53.648519+02:00"
---

Follow-up from the PTX lifecycle migration (771d921a): tools/ptx/ad-gradcheck-launch.f was deferred because its header pins a cross-tree-loadability contract (no requires - it ships to the device box), and a require of lib/ptx/cuda-scope.f breaks that until the device box runs a matching checkout. Either update the ship prefix mechanism to carry cuda-scope, or confirm the file is spark-only and drop the contract; then migrate (outer scope owns ctx + 6 allocations, module pairs stay per-entry mutable slots).
