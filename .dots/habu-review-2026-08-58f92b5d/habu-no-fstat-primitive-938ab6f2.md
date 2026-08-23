---
title: "no fstat primitive: post-open checks re-race the path"
status: open
priority: 2
issue-type: task
created-at: "2026-08-23T12:49:26.590932+02:00"
---

Problem: src/habu/habu1.f:2923 EMIT-FS-PRIMS exposes open, open-rd, access, stat64, lstat64, fcntl, close but no fstat, and src/os/linux/sys.f OS-OPEN-FLAGS masks every flag bit but accmode/APPEND/CREAT/TRUNC (no O_NONBLOCK, no O_NOFOLLOW), so lib/fs.f FS-WRITE-BY-FLAGS can only re-stat the PATH after open: a symlink flipped between a regular file and /dev/null between the two checks makes the post-open check throw past the open descriptor (measured 2026-08-23 by the lib-leaks lane: 1,554 fds leaked in 2,000 raced WRITE-ALL calls; the pre-open guard is what keeps open() from blocking forever on a fifo and must stay). Acceptance: an fstat primitive on both engines (native + bootstrap mirror, with the mirror tripwire row), OS-OPEN-FLAGS passing O_NOFOLLOW (and O_NONBLOCK where a caller needs it) on Linux and macOS, FS-WRITE-BY-FLAGS checking the descriptor it holds, the raced reproducer green with no fd growth; a negative fixture per engine. Files: src/habu/habu1.f, bootstrap/cg/forth.fs, src/os/linux/sys.f, src/os/macos/sys.f, lib/fs.f, lib/fs-test.f. Verify: fs tests; the raced reproducer; recovery gate. Depends: habu-pkg-diff-lint-8bed2604 (lib/fs.f is frozen), habu-fs-write-by-3d0ea649. Ownership: os layer. Claim: unassigned.
