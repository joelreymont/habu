---
title: file-objects readers hold an object in a fixed 8192-byte buffer
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T15:58:46.509650+02:00"
---

Problem: test/file-objects.f SAME-OBJECT, CHECK-STOP and WARM-CUT read build A to D's objects into CAP BUFFER: BYTES-A and BYTES-B (8192 constant CAP). An object holds absolute paths, so a long enough checkout or temp path makes FS:READ-ALL throw E-FS-CAPACITY and the test fails for a reason it does not check. FIXTURE-OBJECT had the same cause and now sizes DYNAMIC-BUFFER FIXTURE-BYTES by FS:FILE-SIZE. Found by the review of habu-file-objects-cases-a0c09cf6 (finding 2). Source: Joel: "i want all issues identified to be dotted and fixed". Acceptance: the three readers size their buffers by the object as FIXTURE-OBJECT does; CAP, BYTES-A and BYTES-B are gone. Files: test/file-objects.f. Verify: bin/hb --load test/file-objects.f rc 0; rg -n 'CAP BUFFER|8192' test/file-objects.f prints nothing. Depends: none. Ownership: test/file-objects.f. Claim: unassigned.
