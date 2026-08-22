---
title: Darwin contracts the engine patches over on Linux
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.931920+02:00"
---

Problem: (a) lib/test/budget.f:28,43-49 T-BUDGET-CAL-REF-MS returns 0 on Linux so the self-calibration is inert on the primary host; (b) lib/process.f:54 F-SETNOSIGPIPE 73 is Darwin's fcntl and src/habu/habu1.f:846-855 intercepts cmd 73 into a process-wide LINUX-IGNORE-SIGPIPE - a per-fd contract delivered globally, keyed on a magic number across layers; (c) lib/fs.f:21-26 stat offsets are Darwin struct stat64 and habu1.f:1975-1981 LINUX-STAT-FIX rewrites the kernel struct into that layout, undocumented in fs.f; lib/memory.f:17 $1002 and mmap-exhaust.f:104 $5002 likewise; (d) lib/codesign.f names signing but on Linux is chmod+x (CODESIGN-LINUX-VERIFY-RC 0); (e) lib/process-test.f:151 and process-env-test.f:274 assert ENOENT only under HB-TARGET-MACOS?. Acceptance: the Darwin-shaped OS ABI documented once in fs.f and docs/porting.md; budget.f gets a Linux reference measured on this host or a structural calibration; the fcntl-73 hijack becomes a named primitive; codesign renamed to what it does; ENOENT asserted on both targets. Files: as listed. Verify: lib tests under bin/hb on Linux. Depends: engine on this host. Ownership: os layer. Claim: unassigned.
