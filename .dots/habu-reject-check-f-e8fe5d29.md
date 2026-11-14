---
title: "Reject check.f's FFI staging overflow with a record"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T22:47:41.940564+01:00"
---

Problem: src/habu/verify-source.f FFI-STAGING stops tools/check.f with a bare E-FFI-ARITY STATEMENT-STOP when a FUNCTION: declaration's struct inputs do not fit (FFI:STRUCT-FITS?), while a load of the same declaration prints src/lib/ffi-abi.f's placed E-FFI-ARITY record with its limit and help. Acceptance: tools/check.f on such a declaration prints the same record, placed at ;FUNCTION, as a check.f test. Depends: none. Claim: unassigned.
