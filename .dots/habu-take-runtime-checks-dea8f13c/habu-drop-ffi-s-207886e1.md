---
title: "Drop FFI's run-time index and record checks"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T00:00:48.498378+01:00"
---

Problem: src/lib/ffi-abi.f checks at every call what the declaration already bounds: FFI-CHECK-INDEX (:98) in FFI-SLOT, FFI-FSLOT and FFI-F32SLOT (:104-112), FFI-CHECK-STACK (:113) in FFI-STACK-SLOT, FFI-STACK-FSLOT and FFI-STACK-AT (:117-128), the KPARAM, FN and LIB slots (:170, :309, :353), FFI-CHECK-COUNT (:887-888), RECORD-CELLS' null and length test (:812), and STRUCT-RESULT's null or misaligned destination (E-FFI-SYNTAX at the call, header :974). FUNCTION:'s staging indexes are literals its planner bounds at ;FUNCTION. Acceptance: default code runs none of these; each bound is checked where the declaration or its caller is read; an instrumented build may add them (a49d054d). Depends: none. Claim: unassigned.
