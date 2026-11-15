---
title: "Raise FUNCTION:'s input cap to C's 127"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T23:57:06.705616+01:00"
---

Problem: src/lib/ffi-abi.f ARG-MAX ($10), with FFI-MAX-ARGS 16 and the 16-cell argument and stack buffers at the top of the file, rejects a declaration past 16 inputs (INPUTS-FULL), a limit of Habu's own; C guarantees a call 127 arguments (C17 5.2.4.1). test/ffi-c-calls.f's mixed case (FFI-T-CC-MIXED) cannot spill both kinds because of it. Acceptance: a declaration of 127 inputs with both kinds past their registers loads and calls right on Linux arm64 (test/ffi-c-calls.f); a 128th input is rejected with E-FFI-ARITY naming C's limit. Depends: none. Claim: unassigned.
