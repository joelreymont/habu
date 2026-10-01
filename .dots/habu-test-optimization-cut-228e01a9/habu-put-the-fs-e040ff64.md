---
title: Put the fs-mutate socket root in HB_SOCK_TMP
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T04:12:40.704763+02:00\""
closed-at: "2026-10-01T17:10:46.583716+02:00"
close-reason: Fixed by wyxvttul 7b4e93c9 (review 57 ACCEPT)
---

Problem: lib/fs-mutate-test.f:198-201 FMT-ROOT! makes its socket root under TMPDIR, else /tmp, outside the row's HB_TMP, and only FMT-REMOVE! on a normal finish reaps it, so a row the pool kills leaves the directory behind. The gate pool now hands each child a short HB_SOCK_TMP that it reaps (lane habu-e3a6b207, r4-pg). Acceptance: under the pool the root is inside HB_SOCK_TMP and a killed row leaves nothing; outside the pool the root is still short enough for sun_path and removed on finish; shown by killing the row under the pool. Files: lib/fs-mutate-test.f. Verify: the fs-mutate rows. Depends: the r4-pg lane (dot e3a6b207). Ownership: FMT-ROOT!. Claim: second commit of the r4-pg lane.
