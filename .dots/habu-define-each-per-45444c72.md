---
title: Define each per-OS libc constant once, in its OS seam
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T00:27:26.671662+01:00"
---

Problem: libc values that differ between macOS and Linux are chosen at each use with 'HB-TARGET-MACOS? if <macOS> else <Linux> then', often as private copies of one value: ENAMETOOLONG and ELOOP (src/lib/fs.f:62-68, ELOOP again at src/lib/fs-cause-test.f:17), EAGAIN (src/lib/fd.f:100, src/lib/test/fifo.f:28), EADDRINUSE (src/lib/net/tcp4-test.f:29), ECONNRESET and RLIMIT_NOFILE (test/http-accept-starved.f:35-37, RLIMIT_NOFILE again at src/lib/net/ws-test.f:1923), AT_FDCWD (src/lib/fs.f:730, src/lib/ffi-abi.f:380), and bare branches at src/lib/net/tcp4.f:122-167, src/lib/net/udp4.f:59, src/lib/serial.f:49, src/lib/fs-mutate.f:564 and src/lib/process-tree.f:70. A value the target seam holds per OS, as src/os/<target>/target.f holds PATH-CAP, needs no branch, and a copy can disagree with the value it repeats. Acceptance: each such value is defined once per OS in the seam and used by name; no 'HB-TARGET-MACOS? if <number>' remains outside src/os/; habu-one-libc-o-e75e8ff6's O_NONBLOCK copies go the same way. Depends: none. Claim: unassigned.
