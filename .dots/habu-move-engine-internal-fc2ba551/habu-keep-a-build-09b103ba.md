---
title: "Keep a build's marks above a nested file's rollback"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T20:19:25.247025+01:00"
---

Problem (inferred from code, not reproduced): a caught evaluate whose text requires a file and then throws rolls back the build's held facts, rows and shadow map to the frame's mark (habu-link-a-build-7dbda6fe), which may lie below the marks the nested file's build took (src/habu/linker.f FILE-HELD, FILE-ROW, FILE-EM, OLD-PATCHES); the loops from those marks (RETIRED!, LAST-HELD!, FACTS+, CODE+, CHECK-EARLIER) would then start above their limit. Acceptance: a test/file-objects.f build of that shape either links with cold equal to warm or is rejected with a record; a probe first shows which happens today. Depends: habu-link-a-build-7dbda6fe. Claim: unassigned.
