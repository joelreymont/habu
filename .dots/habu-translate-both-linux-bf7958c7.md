---
title: "Translate both Linux seams' flags from one list"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T19:23:39.883991+01:00"
---

Problem: src/os/linux/sys.f (aarch64) and src/os/linux-x86-64/sys.f keep their own hand-written open and mmap flag translations and nothing checks one against the other: x86-64 OS-MMAP-FLAGS (:194-200) keeps $13 and MAP_ANON and drops MAP_NORESERVE (BSD $40, Linux $4000), which src/habu/linker.f:5040 MAP sets, so once x86-64 builds run on the linker (docs/bootstrap.md:24-25) its reservations are charged at map time. Acceptance: each seam emits its open and mmap translations from one list of BSD and Linux bit pairs both read; x86-64 maps MAP_NORESERVE; test/x86-64-emit.f MMAP-CASES checks its group. Depends: none. Claim: agent=kevin workspace=.jj-ws.noindex/kevin-flags.
