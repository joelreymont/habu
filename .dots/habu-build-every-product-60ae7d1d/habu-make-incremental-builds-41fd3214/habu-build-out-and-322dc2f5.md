---
title: Build-out and engine-id caches a capture hook clears are carried by objects
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:21:34.698867+02:00"
closed-at: "2026-11-10T12:53:35.406280+02:00"
close-reason: "landed with the commit: the build-out and engine-id caches are scratch cells; before it P2's program printed the builder's bin/hb as its own path, after it its own; review ACCEPT; engine three generations equal, suite 270 of 270. Follow-ups: habu-file-objects-f-47d24827 (the test does not assert the link), the shared scratch declaration in habu-design-no-file-298feb05"
---

Problem (code review of the registration-cells fix, 2026-11-10, by reading, not reproduced): src/lib/build-out.f:19-21 OUT-U, OBJ-U, OUT-BUF, OBJ-BUF, PATH-BUF and src/lib/engine-id.f:20-24, :46-47 EID-PATH-U, EID-PATH-DONE, EID-KEY-U, EID-KEY-DONE, EID-PATH, EID-KEY, EID-PATHZ, EID-MAPPED, EID-OPENED are plain cells, so a file object carries them, while the IMAGE-LIFECYCLE:REGISTER hook that clears them (build-out.f:32, engine-id.f:79) is never carried. REGISTER-CLEANUP is reached only while a cache is zero (build-out.f:53, engine-id.f:102, :160). A program file whose top level calls ENGINE-ID:PATH$, ENGINE-ID:KEY$ or BUILD-OUT:BASE$, linked from its object while app-end.f compiles again (build it, build a no-require program, build it again in one out root), would leave PREPARE nothing to run and write a program that starts with the build hb's path and key cached. Acceptance: a fixture whose top level calls ENGINE-ID:PATH$ and whose MAIN prints ENGINE-ID:PATH$ prints the program's own path after that three-build order, and the test fails before the fix; every cell the two hooks clear is a scratch cell (a buffer marked cell by cell as src/core/include.f SCRATCH-BYTES does). Files: src/lib/build-out.f, src/lib/engine-id.f, test/file-objects.f and a fixture. Verify: the fixture case in test/file-objects.f after build L2; engine built twice equal. Depends: none. Ownership: those files. Claim: unassigned.

Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cache.
