---
title: A semaphore a top level initialised is carried
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:42:56.190507+02:00"
---

Problem (by reading, not reproduced, 2026-11-10): src/lib/task.f:2107 TASK:SEMAPHORE allots its record plain (create TASK-SEMAPHORE-BYTES allot), so a semaphore a file's top level initialised is carried by the file's object into the next build and the program: a Mach port name of the building process on macOS, a sem_t image on Linux. TASK:FACILITY is the precedent: its record is all scratch. Acceptance: first a reproducer as a case of test/file-objects.f (a file whose top level declares a SEMAPHORE and initialises it, built cold, a no-require program built, the first built again in one out root; the program waits on or signals the semaphore; record what the cold and the warm program do); then the record is scratch storage, so the semaphore reads uninitialised in a later file and in the program, or the report says why it must be carried. Files: src/lib/task.f, test/file-objects.f. Verify: the case in test/file-objects.f; src/lib/task-test.f. Depends: the O8 task lanes landing (dot habu-task-cells-a-5a896f0c). Ownership: src/lib/task.f TASK:SEMAPHORE. Claim: unassigned.
