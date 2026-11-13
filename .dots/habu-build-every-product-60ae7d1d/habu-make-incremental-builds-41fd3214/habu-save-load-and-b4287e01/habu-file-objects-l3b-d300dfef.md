---
title: "File objects L3b: no object for a file that caught an evaluate's throw"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T22:56:08.950772+02:00"
closed-at: "2026-11-06T23:38:41.419710+02:00"
close-reason: "refuted, no tree edit: the file buffer's clean exit clears EVALERR-CELL (src/habu/habu2.f:12834 EM-EVAL-CLEAN-EXIT; src/habu/interpret.f:141-143 CLEAN-END) before LOAD-CURRENT reads it (src/core/include.f:2014), so a file whose top level caught an evaluate's throw does not end the process there and its object matches its run (probe /tmp/carl-l3b/)."
---

Problem: LOAD-CURRENT (src/core/include.f:2008-2014) ends the process with "include: evaluation failed" when EVALERR-CELL is set, that is when a throw escaped an evaluate frame and the file's own top level caught it (habu2.f:12228-12230). RUN-FILE (src/habu/linker.f:811-829) has already written that file's object by then, so once L4 reuses objects a warm build links the object and the death is skipped: a warm build differs from a cold one. Inferred from the code, not reproduced. Acceptance: by the lead's 2026-11-06 ruling that a failed build does not go on (once a file's load throws, the linker loads no other file and writes neither that file's object nor the program, refusing with LINKER:E-FAILED), RUN-FILE refuses such a file as it refuses DP moved back: the step is set to failed, the build ends LINKER:E-FAILED, and no object is written for the file. Lane after L3 lands, in two briefs: tests first (a fixture whose top level catches a raw evaluate's throw; a build.f stop case expecting E-FAILED and no object; today's behaviour is the reproducer), then the RUN-FILE check. Files: src/habu/linker.f, test/file-objects.f, test/fixtures/file-objects/. Verify: bin/hb --load test/run.f -- linker file-objects. Depends: L3. Ownership: carl (lead), worker lane. Claim: unassigned.
