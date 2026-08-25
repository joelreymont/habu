---
title: Deduplicate hook pair match in trusted inventory
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T23:57:33.310445+02:00"
---

Full context: found by agent hookpath 2026-07-30. Any future consumer of the (file, hook name) registry must call HOOK-SITES:CHECK-MATCH?, the exported predicate tools/checked-boundary-lint-core.f uses (commit 19cec81e), rather than re-implementing the pair match inline; duplicated authority is exactly the defect class that produced three false positives in the boundary lint. What remains here is the one clean measurement the hookpath lane could not take: checked-boundary-lint-core.f now requires tools/hook-sites.f, so every hb check load compiles the registry - measure the load-time delta on a quiet host and record it (expected negligible; if it is not, the registry needs a leaner load shape).
