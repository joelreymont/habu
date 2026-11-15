---
title: "Drop FFI's run-time index and record checks"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T00:00:48.498378+01:00"
---

Problem: src/lib/ffi-abi.f checks at every call what the declaration already bounds: FFI-CHECK-INDEX in FFI-SLOT, FFI-FSLOT and FFI-F32SLOT, FFI-CHECK-STACK in FFI-STACK-SLOT, FFI-STACK-FSLOT and FFI-STACK-AT, the KPARAM, FN and LIB slots (FFI-KPARAM-CHECK, FN-CHECK, LIB-CHECK), FFI-CHECK-COUNT in DECLARE-ROW, RECORD-CELLS' null and length test, and STRUCT-RESULT's null or misaligned destination (E-FFI-SYNTAX at the call, as the FUNCTION: header says). FUNCTION:'s staging indexes are literals its planner bounds at ;FUNCTION. Acceptance: default code runs none of these; each bound is checked where the declaration or its caller is read; an instrumented build may add them (a49d054d). Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-band.
