---
title: FS-WRITE-BY-FLAGS leaks the fd on refusal
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.921224+02:00"
---

Problem: lib/fs.f:311-314 checks FILE? before open and again after it; the second check throws E-FS-OPEN past the open fd, so every WRITE-ALL/APPEND-FILE on a fifo, device, or a path raced into a directory leaks one descriptor; the runner and gate pools call these in loops. Acceptance: close FS-IO-FD before the throw (the shape two lines below), drop the pre-open duplicate; a test writes to a fifo path and asserts the fd count unchanged. Files: lib/fs.f, lib/fs-test.f. Verify: the test. Depends: none. Ownership: fs. Claim: unassigned (released 2026-08-23; see the correction below).

NOT landed in this lane; contract corrected by the lane's measurement: the pre-open FILE? guard is what keeps open() from blocking forever on a fifo and must STAY; the leak is the TOCTOU race between the two checks (1,554 fds in 2,000 raced WRITE-ALL calls); the fix is `FS-IO-FD @ close` before the post-open throw plus the raced reproducer as the test; a descriptor-level check needs the fstat primitive (habu-no-fstat-primitive-938ab6f2). Blocked on lib/fs.f being frozen (267-file seal cascade) until habu-pkg-diff-lint-8bed2604 lands. Claim released 2026-08-23; reopen as a two-line lane after the lint policy.
