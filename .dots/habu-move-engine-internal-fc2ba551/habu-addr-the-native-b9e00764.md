---
title: Address the native origin record without a lookup
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.870926+01:00"
---

Problem: src/habu/native-body.f BODY-CAPABILITY finds the kernel primitive native-origin-cell by name in WL:PRIMITIVES at run time, checks its entry lies in kernel code, and runs it to get the origin record's address, a fixed DATA place (src/habu/layout.f, after CALLBACK-ENTRY-CELL). Acceptance: NATIVE-BODY addresses the record at its layout.f cell with no lookup and no run-time check; the arm64 native-origin-cell primitive and its row are deleted (the x86-64 kernel's goes at the x86-64 step); interpreter.f CLEAR-BODY-TAKE works as before. Depends: none. Claim: unassigned.
