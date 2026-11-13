---
title: Address the native origin record without a lookup
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.870926+01:00"
---

Problem: src/habu/native-body.f BODY-CAPABILITY finds the kernel primitive native-origin-cell by name in WL:PRIMITIVES at run time, checks its entry lies in kernel code, and runs it to get the origin record's address, a fixed DATA place (src/habu/layout.f, after CALLBACK-ENTRY-CELL). Acceptance: NATIVE-BODY addresses the record at its layout.f cell with no lookup and no run-time check; native-origin-cell, its row and its arm64 and x86-64 bodies are deleted; interpreter.f CLEAR-BODY-TAKE works as before. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-origin.
