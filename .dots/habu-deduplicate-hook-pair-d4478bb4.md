---
title: Deduplicate hook pair match in trusted inventory
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T23:57:33.310445+02:00"
---

Full context: found by agent hookpath 2026-07-30. tools/trusted-inventory.f:776-782 re-implements the (file, hook name) registry pair match inline instead of calling HOOK-SITES:CHECK-MATCH?, the exported predicate that tools/checked-boundary-lint-core.f now uses (commit 19cec81e). Duplicated authority is exactly the defect class that produced three false positives in the boundary lint - the inline copy can drift the same way. Replace the inline match with the registry predicate, keep the census output byte-identical on the real tree, and add a mutation check: skewing the registry (removing one row) must change the census verdict the same way it changes the boundary lint's. While there, take the one clean measurement the hookpath lane could not: checked-boundary-lint-core.f now requires tools/hook-sites.f, so every hb check load compiles the registry - measure the load-time delta on a quiet host and record it (expected negligible; if it is not, the registry needs a leaner load shape).
