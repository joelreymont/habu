---
title: Give O-NONBLOCK its Linux value in lib/process.f
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T15:13:59.222405+03:00\""
---

Problem: lib/process.f:66 '4 constant O-NONBLOCK' is the macOS value; Linux/aarch64 O_NONBLOCK is $800 (0o4000), so PROC-NONBLOCK! (lib/process.f:389, used at :410 on the child's stdin write end PROC-IN-W) sets O_NONBLOCK on macOS only and on Linux ORs in an access-mode bit F_SETFL ignores, a silent no-op: a parent feeding a child's stdin blocks in write once the pipe is full and the child stops reading, and a library that arms the signal stub's self-pipe cannot make its write end non-blocking (measured by the signal-stub lane 2026-09-17: a read on a 'non-blocking' pipe blocked forever). Every other Linux/macOS split in lib selects by HB-TARGET-LINUX? / HB-TARGET-MACOS? (lib/process-pty-io.f:243, lib/fs.f:413). Acceptance: O-NONBLOCK is target-selected ($800 on Linux, 4 on macOS, E-PROC-HOST otherwise); a regression reads the flag back through F_GETFL after PROC-NONBLOCK! and shows a write into a full pipe returning EAGAIN instead of blocking; the stdin writer that relies on it (the PROC-STDIN-CHUNK loop) handles EAGAIN and short writes by waiting on POLLOUT under its deadline, so a child that stops reading no longer wedges the parent; the process and capture suites green. Files: lib/process.f, the process tests under test/. Verify: the process suites named in test/gate-stdlib-cases.f for lib/process.f, plus the new regression. Depends: none. Ownership: lib/process.f. Claim: agent=hazel-nonblock workspace=.jj-ws/hazel-nonblock.
