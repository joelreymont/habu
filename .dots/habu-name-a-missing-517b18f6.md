---
title: Name a missing HB_TMP directory
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T08:39:00.851397+03:00"
---

Problem: with HB_TMP naming a directory that does not exist, a test that makes scratch dies with only 'hb: cleanup at exit threw -2105' and 'hb: uncaught throw code -2105', rc 67 (measured: HB_TMP=<missing> bin/hb --load src/lib/fs-mutate-test.f). -2105 is E-FS-IO (src/lib/errors.f:29). src/lib/fs-mutate.f:583 HB-TMP-MKDIR is the one word that reads HB_TMP to make scratch: it hands the missing parent to MAKE-TEMP-DIR, and nothing says HB_TMP is the cause. Acceptance: a missing HB_TMP directory is refused with a message that names HB_TMP and its path, before any scratch is made, and the process exits nonzero. A reproducer row (a child run with HB_TMP naming a missing directory) asserts the message and the nonzero exit. An existing HB_TMP and an unset HB_TMP behave as before. Files: src/lib/fs-mutate.f and its test. Verify: the fs-mutate rows and the new case. Depends: none. Ownership: src/lib/fs-mutate.f, src/lib/fs-mutate-test.f. Claim: unassigned.
