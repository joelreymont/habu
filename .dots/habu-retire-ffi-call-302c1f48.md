---
title: "Retire ffi-call-n on x86-64"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T00:15:29.476382+01:00"
---

Problem: on ARM64 ffi-call-n runs ffi-call's body (src/habu/habu1.f BFFI-CALL) and FFI:DECLARE rejects more than eight arguments, but the x86-64 twin still copies the cells past eight to the stack (src/habu/kernel-x64.f FFI-CALL-N-BODY :4017-4020, registered in FFI, at :4057, and STACK-CELLS, :3986-3991), so src/habu/prims.f keeps two rows, ffi-call and ffi-call-n, for one call. Acceptance: at x86-64's turn one primitive row passes x0-x7 (rdi..r9 and two stack cells) on both targets, its x86-64 body copies no further cell, and ffi-abi.f calls it; test/x86-64-kernel-ffi.f follows. Depends: none; it waits for x86-64's turn. Claim: unassigned.
