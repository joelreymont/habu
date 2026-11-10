---
title: Task cells a capture hook clears are carried by objects
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:21:34.709801+02:00"
---

Problem (code review of the registration-cells fix, 2026-11-10, by reading, not reproduced): in src/lib/task.f these are plain cells an object carries while the hook that resets them is never carried. (b) :602-604 MAIN-PARK-REC and MAIN-PARK-READY: 2 records the main task's semaphore (a Mach port name on Darwin) and its IMAGE-LIFECYCLE:REGISTER of MAIN-PARK-RESET (:624-637); a file whose top level parks or wakes the main task would give a warm link another process's semaphore. (c) heads of process-local mapped chains swept by hooks: TASK-SEM-POOL's link and used cells (:448), TCB-POOL (:1179), TCB-CHUNKS (:1181), EXIT-CHUNKS (:759). (d) callback rows' per-row process state ROW-REGION, ROW-MOVES, ROW-CLAIM (cleared by ROWS-SWEEP :1255-1265): a warm link replays rows and nothing arms the sweep; the public word ARM-IMPORTED-CALLBACKS (:2244) was written for this and has no caller in src, tools, test, docs or ../etch. Acceptance: a reproducer for each of b, c and d (a program whose top level does the thing, built, a no-require program built, the first built again in one out root; what the third build or its program does); then each cell a hook clears is a scratch cell, or the report says why one must be carried; ARM-IMPORTED-CALLBACKS is called where a link needs it or removed. Files: src/lib/task.f. Verify: the reproducers as cases of test/file-objects.f; src/lib/task-test.f; engine built twice equal. Depends: none. Ownership: src/lib/task.f. Claim: unassigned.

Hunt (2026-11-10, each by a mixed build): (b) top level `TASK:SELF TASK:WAKE`: three builds rc 0, the third program dies -3606 (E-TASK-SEM-STATE), rc 67; replayed MAIN-PARK-REC's guard cell, its Mach port name and MAIN-PARK-READY = 2. (c) top level `TASK:NEW ... drop`: the cold build is refused rc 74 'task at capture', the third build passes rc 0 and its program dies signal 11; replayed TASK-SEM-POOL's used cell, TCB-POOL and TCB-CHUNKS; scratch marks cannot restore the refusal. (d) top level declares and binds a CALLBACK: all rc 0 and the programs print the same; ARM-IMPORTED-CALLBACKS has no caller. Case b is fixed in this dot; c and d wait for design habu-design-no-file-298feb05.

Claim: agent=jerry workspace=.jj-ws.noindex/jerry-task.
