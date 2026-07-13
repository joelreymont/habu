---
title: Census CAD effect cache identities
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:28:00.753509+02:00"
blocks:
  - habu-resolve-runtime-cad-2864336f
---

Full context: R8 requires every cache and promotion key reachable from Model CAD execution to include or deliberately reject resolved semantic effects, but current ownership spans schedule replay, compilation/artifact lookup, result caches, persistent store rows, and promotion. Perform a read-only fixed-string and call-graph census from Maki op registration/execution through sched-key, store/replay, artifact compilation, result lookup, evidence, and promotion. For every lookup/write, record key producer, payload, effect reachability, persistence schema, migration policy, and owning tests; exclude host caches that cannot observe Model CAD invocations with evidence. Split disjoint exact-file migration dots and amend habu-key-caches-by-fddcea19 to block every leaf before closing. Acceptance: committed MODEL-CAD-V2-PLAN.md table, every reachable cache/publication owned exactly once, old-row reject/migrate policy explicit, no source consumer edits, dot-dep/host/filemap/status green. Depends on the frozen runtime binding resolver contract.
