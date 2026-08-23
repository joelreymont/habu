---
title: FS-WRITE-BY-FLAGS leaks the fd on refusal
status: active
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.921224+02:00"
---

Problem: lib/fs.f:311-314 checks FILE? before open and again after it; the second check throws E-FS-OPEN past the open fd, so every WRITE-ALL/APPEND-FILE on a fifo, device, or a path raced into a directory leaks one descriptor; the runner and gate pools call these in loops. Acceptance: close FS-IO-FD before the throw (the shape two lines below), drop the pre-open duplicate; a test writes to a fifo path and asserts the fd count unchanged. Files: lib/fs.f, lib/fs-test.f. Verify: the test. Depends: none. Ownership: fs. Claim: agent=lib-leaks workspace=.jj-ws/habu-fs-write-by-3d0ea649
