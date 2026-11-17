---
title: "Check FUNCTION:'s library in the verifier"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T09:25:16.142086+01:00"
---

Problem: a FUNCTION: with no library selected is rejected by the load with E-FFI-LIBRARY, while check.f and --verify-only do not check the selection (src/habu/verify-source.f RECORD-FFI-FUNCTION; reproducer: a file holding `require src/lib/ffi-abi.f` then `FUNCTION: F getpid ( -- i32 )` prints E-UNCLOSED there; with `;FUNCTION` added --verify-only passes). The load checks the selection's wordlist against get-current (src/lib/ffi-abi.f CHECK-SCOPE), and a selection may be made by a word that calls FFI:SELECT-LIBRARY (src/lib/ffi-test.f:600), so the verifier must know which top-level calls select, as it knows which move the source root. Acceptance: bin/hb FILE, check.f and --verify-only print one and the same E-FFI-LIBRARY record, rc 70. Depends: none. Claim: unassigned.
