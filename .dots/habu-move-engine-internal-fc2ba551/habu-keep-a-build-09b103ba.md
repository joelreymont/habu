---
title: "Link or reject a rollback that gives back a required file"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T20:19:25.247025+01:00"
---

Problem: a caught evaluate whose text requires a file and then throws gives back what that file's part holds: its records, rows and the data its load allotted, below the marks the linker took for the parts it wrote (src/habu/linker.f FILE-REC, FILE-ROW, FILE-EM, FILE-HELD). test/fixtures/file-objects/rollback.f FOB-REQ is refused (rc 67, 'it gave back data space') because MARKS's POINTER-STORAGE:ALIGN-APART allots at each part start, so the rollback lowers DP; a DATA-FLOOR:RAISE during the load (src/lib/task.f:803, src/habu/intern.f:141) may lift the rollback's floor and leave DP where it was, and the record names data space, not the given-back file. Acceptance: a test/file-objects.f build of that shape either links with cold equal to warm or is rejected with a record naming the given-back file, whatever DP the rollback leaves; a probe first shows whether a load that raises the data floor links such a build. Depends: none. Claim: unassigned.
