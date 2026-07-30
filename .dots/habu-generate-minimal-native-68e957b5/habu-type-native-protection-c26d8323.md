---
title: Type native protection state
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T11:52:19.518562+02:00"
blocks:
  - habu-define-typed-arm64-4ab8894f
---

Context: src/habu/habu2.f manually emits LPROT RW/RX transitions, so callers can emit redundant flips and the checker sees neither the current region mode nor transition legality. Fix: add package-scoped region<rw>/region<rx> machine effects and transition operations consumed by native emitter construction; same-state transitions canonicalize to no operation and writes/calls requiring the wrong state reject before encoding. Keep runtime-conditional state explicit instead of guessing. Acceptance: negative fixtures reject emission in the wrong mode and unjoined branch states; positive fixtures prove identical-state transitions emit zero bytes; EM-COMPILE-CALL no longer needs a manual redundant RW flip. Direct prerequisite: habu-define-typed-arm64-4ab8894f supplies the shared effect schema. Files: src/core/type-family.f, src/arch/arm64/icode.f, src/habu/habu2.f, test/engine-suite.f. Verify: focused checker negatives, byte-exact emitter fixture, native fixpoint.

Claim: agent=a64effects workspace=.jj-ws/habu-define-typed-arm64-4ab8894f
