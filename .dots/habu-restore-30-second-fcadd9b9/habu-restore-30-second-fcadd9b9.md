---
title: Restore 30-second native gate
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-17T13:24:55.150379+02:00\""
---

Stop-line regression: current master test/run.f needs 47-49s and hard-fails the 44s nominal performance verdict; historical operational range was 25-30s. Measure stage spans and process/cache counts on exact master, identify the first regressing revisions, remove redundant compilation/testing work at the architectural owner, and preserve correctness/fail-closed behavior. Add a hard nominal 30s regression contract plus stage budgets that fail on the first offending change rather than permitting baseline drift. Acceptance: three isolated cold-cache exact-tree runs are <=30s nominal on the calibrated macOS profile; warm/cache-hit paths have explicit lower budgets; current full native, maki, ptx-stdlib, recovery/fixpoint, host/dot gates pass. Files: test/run*.f and measured owning compiler/test files only. Depends on no unrelated milestone work; this stop-line repair must land before M1 resumes.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim. Parent of the gate stop-line repairs; the measurable work is in its children.
