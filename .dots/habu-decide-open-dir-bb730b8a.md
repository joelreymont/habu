---
title: Decide OPEN-DIR-AT and READ-FILE-AT
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T10:40:49.290225+02:00"
---

Problem: FS:OPEN-DIR-AT and FS:READ-FILE-AT (src/lib/fs-anchored.f) have no production caller; programs outside this tree call the file's other public words. READ-FILE-AT is the portable reader the kept words' tests check through (src/lib/fs-anchored-test.f READ$, READ-CHECK, READ-CHECK-CLOSE; test/file-objects.f ANCHORED-CASE), and the only kept file opens are Linux-only. Acceptance: each has a production caller or is deleted, with the kept words' tests reading through a word that stays, on every target, and no private code left unused. Depends: none. Claim: unassigned.
