---
title: Make EVALUATE a full compiler transaction
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-14T19:39:00.406749+02:00\""
blocks:
  - habu-restore-catch-return-1074ce3f
  - habu-owner-seal-persist-1f23e205
---

Full context: src/habu/habu1.f B-EVAL and src/habu/habu2.f EM-EVAL-THROW-RECOVER currently checkpoint only input/CP/NDICT/DP. A throw from nested EVALUATE during an immediate word destroys the outer in-progress definition because EM-RESET-COMPILE-STATE clears its scalar/allocator/control state; handler equality/strict-lower ownership alone cannot resume it. Separately, evaluated ': TX-A ... ; -123 throw' rolls back the runtime dictionary so search-wl is absent, but checker/type-family certification metadata leaks and a later valid TX-A definition exits 78 duplicate. `s" package EP -123 throw" evaluate` also leaves CURRENT at the abandoned package WID and leaks WID/owner/protected registry allocations. Implement one complete EVALUATE transaction with nested checkpoint stack and explicit commit/rollback: preserve/restore every compiler-owned scalar and live control structure required to resume outer compilation; CUR/WIDN/DEF-WL/package pub/pri/parent/record plus owner/protected registry marks; and checker/type-family/type-schema/related declaration metadata through existing transactional APIs or new complete checkpoint APIs. Prefer a sealed internal raw-evaluate primitive and public transactional evaluate wrapper with explicit RBF commit/rollback hooks; prove direct/tick access to raw-evaluate is sealed. Catch owns data/return/loop/handler/machine stacks; EVALUATE owns input, EVALD, compiler/dictionary/data allocation, namespace allocation, and certification registries. Add exact native regressions for nested immediate/evaluate/catch resumption, escaped certified-definition redeclaration, CURRENT restoration with deterministic WID reuse/no registry leak, repeated/nested drift tests, native/bootstrap codegen parity, recovery-host execution proof, snapshot/task/layout/protection updates, checked diff lint, owning gates, and full fixpoint. Do not cap, shrink, reset globally, or bypass certification.
