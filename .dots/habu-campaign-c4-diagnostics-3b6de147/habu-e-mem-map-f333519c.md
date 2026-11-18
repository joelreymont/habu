---
title: E-MEM-MAP throws carry no record
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T08:42:19.396354+01:00"
---

Problem: a refused map throws E-MEM-MAP (-3201, src/core/engine-error.f:16) with no record from src/habu/linker.f:5079/5081, src/lib/memory.f:44/295/305 and src/lib/heap.f:26/34, beside DYNAMIC-STORAGE:E-MAP (7138), which carries one (src/core/dynamic-storage.f:67-74): two codes for one failure, and hb-build under ulimit -v 536000000 prints only 'the kernel build's open threw -3201 E-MEM-MAP'. Refusing the mmaps of a 3000-definition check one at a time (strace -e inject=mmap:error=ENOMEM:when=K) prints E-MAP with no 'for' or E-MEM-MAP placed at the program's first line, and exits 67 (UNCAUGHT, a code outside 1..255). Acceptance: one code for a refused map, every throw of it carrying a record with the bytes, what they were for and the ulimit help, and exiting 78 (SYSTEM); callers that catch it (src/lib/log.f:111, tools/hb-build-core.f:455) keep going on it. Depends: none. Claim: unassigned.
