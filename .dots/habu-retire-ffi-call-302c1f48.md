---
title: Keep one row for the eight-cell C call
status: open
priority: 2
issue-type: task
blocks:
  - habu-trusted-dies-prim-4fd12d60
created-at: "2026-11-15T00:15:29.476382+01:00"
---

Problem: src/habu/prims.f keeps two rows for one call, run by one body on both targets (src/habu/habu1.f BFFI-CALL, src/habu/kernel-x64.f FFI-CALL-BODY): ffi-call, public and trusted-only, which no Habu program calls outside tests, and ffi-call-n, private to FFI, which src/lib/ffi-abi.f calls. A trusted-only row has no caller once TRUSTED: is gone. Acceptance: one row, which ffi-abi.f calls; the tests that name ffi-call (test/x86-64-kernel-ffi.f, -task.f, -callback.f, test/diag-position-test.f's authority cases) follow. Depends: habu-trusted-dies-prim-4fd12d60. Claim: unassigned.
