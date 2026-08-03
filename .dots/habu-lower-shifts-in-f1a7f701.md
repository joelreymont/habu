---
title: Lower shifts in the JIT
status: active
priority: 1
issue-type: task
created-at: "2026-08-04T00:51:38.677059+02:00"
---

Why: native jitdump of the exact SHA-256 consumer proves lshift/rshift are absent from EM-COMPILE-ARITH-OPS, so every compiled shift falls through EM-COMPILE-CALL, executes LVSPILL, and caps the correct source-only SHA core at 11.25 MiB/s versus its required 35.7 MiB/s. Result: add lshift and rshift to the existing integer binary VOP path. Both-constant operands fold at JIT time; runtime-value/literal-count and runtime-value/runtime-count forms emit direct AArch64 LSLV/LSRV through the existing virtual-register allocator, with no spill/call fallback when operands are forceable and unchanged cell-count semantics for 0, 63, 64, 65, and negative counts. Preserve the existing atomic allocation-failure fallback. Mirror the exact emitter and dispatch changes in bootstrap/cg/jit.fs and bootstrap/cg/forth.fs. Owner: src/habu/jit.f and src/habu/habu2.f arithmetic VOP implementation, their two bootstrap mirrors, and the existing engine-suite arithmetic fixtures only. Mandatory checkpoint before substantive edits: retain jitdump red for literal and register counts showing BL/call fallback, and prove a representative VOP registration passes the cheapest bootstrap-mirror/source gate. Acceptance: focused engine-suite behavior covers constant fold, literal count, register count, boundary-count parity, and surrounding live-register preservation; post-change jitdump shows native LSLV/LSRV and no BL/LVSPILL path in those exact words; bootstrap mirror/codegen gates, native candidate engine suite, forced byte-identical fixpoint, full native, Maki, and PTX slices pass. Forbidden: SHA-specific primitive, architecture branch outside the existing AArch64 JIT emitter, new compiler framework/state, API/type change, compatibility/versioning, lint/manifest/suite/benchmark framework, generated artifact, TRUSTED boundary, or unrelated optimization.

Frozen VOP design: parameterize the existing LVBINIPREP/VOPI-ENTRY immediate maximum instead of copying its probe; plus/minus keep 4095, shifts pass 63. Literal counts 0..63 emit LSLI/LSRI; every other non-folded count uses LSLV/LSRV so primitive modulo-cell semantics stay exact.

Frozen ownership correction: make only C-VBINI-PROBE through EMIT-VBINIPREP, VOPI-ENTRY through EI-, EMIT-JIT, EM-COMPILE-ARITH-OPS, and EM-COMPILE-OPS private in the existing ENGINE-EMIT package. Keep FESK2, FESK6, and VOP-ENTRY global. The bootstrap mirrors remain Gforth source and receive behavior changes only. Publish no new ENGINE-EMIT word.

Claim: agent=codex workspace=.jj-ws/habu-lower-shifts-in-f1a7f701
