---
title: "Retire ffi-call-n's stack copy"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T00:15:29.476382+01:00"
---

Problem: after rmpzkwrq every FUNCTION: call past x7 rides ffi-call-abi, so ffi-call-n's copy of argument cells 8-15 to the native stack (src/habu/habu1.f BFFI-CALL-N-CORE :2068-2083, its comment :2060; the x86-64 twin src/habu/kernel-x64.f :4016-4057) is reached only through FFI:DECLARE with more than eight arguments, which no caller passes (src/lib/crypto/evp.f passes 5, src/lib/task-test.f 1). Two stack paths stay where one is used. Acceptance: ffi-call-n loads x0-x7 only and copies no stack; FFI:DECLARE rejects more than eight arguments where its argument count is read; the x86-64 twin waits for x86's turn. Depends: none. Claim: unassigned.
