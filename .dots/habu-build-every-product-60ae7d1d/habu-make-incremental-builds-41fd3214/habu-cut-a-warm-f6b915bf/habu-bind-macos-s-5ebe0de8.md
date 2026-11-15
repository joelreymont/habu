---
title: "Bind macOS's realpath, not its legacy entry"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T04:45:37.265578+02:00"
closed-at: "2026-11-13T04:51:31.811852+02:00"
---

Problem: src/habu/habu1.f REALPATH (:2871) DLSYMs "realpath", which on macOS is libc's legacy entry: for a relative path it works out the working directory by reading every parent directory, 4.3 ms for "." under /private/tmp (5611 entries) against 11 us for realpath$DARWIN_EXTSN, the symbol the C toolchain binds. The 168 objects of an engine build that record the read of "." (src/core/include.f:242 CWD-RECORD) make a warm engine build from a plain copy under /tmp 0.7 s slower, and Zen's warm build about 1.0 s. src/habu/kernel-x64.f:1004-1033 has the same name (macOS, parked with x86-64). Acceptance: on macOS REALPATH binds realpath$DARWIN_EXTSN, Linux keeps realpath; a warm engine build from a plain /tmp copy costs what one from the workspace does. Depends: none. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-rh.
