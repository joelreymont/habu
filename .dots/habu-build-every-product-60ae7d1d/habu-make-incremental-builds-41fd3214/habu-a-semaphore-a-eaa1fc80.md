---
title: A semaphore a top level initialised is carried
status: closed
priority: 3
issue-type: task
created-at: "2026-11-10T14:42:56.190507+02:00"
closed-at: "2026-11-14T10:47:57.784292+01:00"
---

Problem (by reading, not reproduced): src/lib/task.f TASK:SEMAPHORE (:1935-1938) allots its record plain (create TASK-SEMAPHORE-BYTES allot), so a semaphore a file's top level initialised is carried by the file's object into the next build and the program: a Mach port name of the building process on macOS, a sem_t image on Linux. TASK:FACILITY is the precedent: its record is all scratch (:1888-1892). Acceptance: first a reproducer as a case of test/file-objects.f (a file whose top level declares a SEMAPHORE and initialises it, built cold, a no-require program built, the first built again in one out root; the program waits on or signals the semaphore; record what the cold and the warm program do); then the record is scratch storage, so the semaphore reads uninitialised in a later file and in the program, or the report says why it must be carried. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-sem.
