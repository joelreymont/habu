---
title: "File objects L3b: no object for a file that caught an evaluate's throw"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T22:56:08.950772+02:00"
blocks:
  - habu-file-objects-l3-7d856048
---

Problem: LOAD-CURRENT (src/core/include.f:2008-2014) ends the process with "include: evaluation failed" when EVALERR-CELL is set, that is when a throw escaped an evaluate frame and the file's own top level caught it (habu2.f:12228-12230). RUN-FILE (src/habu/linker.f:811-829) has already written that file's object by then, so once L4 reuses objects a warm build links the object and the death is skipped: a warm build differs from a cold one. Inferred from the code, not reproduced. Acceptance: by DECISIONS 2026-11-06 'a failure caught inside a build', RUN-FILE refuses such a file as it refuses DP moved back: the step is set to failed, the build ends LINKER:E-FAILED, and no object is written for the file. Lane after L3 lands, in two briefs: tests first (a fixture whose top level catches a raw evaluate's throw; a build.f stop case expecting E-FAILED and no object; today's behaviour is the reproducer), then the RUN-FILE check. Files: src/habu/linker.f, test/file-objects.f, test/fixtures/file-objects/. Verify: bin/hb --load test/run.f -- linker file-objects. Depends: L3. Ownership: carl (lead), worker lane. Claim: unassigned.
