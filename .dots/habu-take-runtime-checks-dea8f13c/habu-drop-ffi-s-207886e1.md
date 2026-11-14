---
title: "Drop FFI's run-time index and record checks"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T00:00:48.498378+01:00"
---

Problem: src/lib/ffi-abi.f checks at every call what the declaration already bounds: FFI-CHECK-INDEX (:94) in FFI-SLOT, FFI-FSLOT, FFI-F32SLOT, FFI-STACK-SLOT and FFI-STACK-FSLOT (:100-114), the KPARAM, FN and LIB slots (:145, :284, :328), FFI-CHECK-COUNT (:841-842), RECORD-CELLS' null and length test (:788), and STRUCT-RESULT's null or misaligned destination (E-FFI-SYNTAX at the call, header :934). FUNCTION:'s staging indexes are literals its planner bounds at ;FUNCTION. Acceptance: default code runs none of these; each bound is checked where the declaration or its caller is read; an instrumented build may add them (a49d054d). Depends: none. Claim: unassigned.
