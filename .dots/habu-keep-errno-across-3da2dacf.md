---
title: "Keep errno across a row's first call"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:01:04.545552+02:00"
---

Problem: src/lib/ffi-abi.f:462-470 FN-LOOKUP resolves a FUNCTION: row at its first call, after a caller cleared errno and before the C call, and the loader may change errno even when it succeeds (:460). src/lib/process-tree.f (:156, :210, :550, :641) and src/lib/fs.f FD-NAME-MAX (:731) clear errno and then read the call's failure from it, so a first call can read the loader's errno; each binds libc's errno location itself. Acceptance: a row's first call leaves errno as the caller set it; one public FFI word clears errno, and both files use it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-ffi-errno.
