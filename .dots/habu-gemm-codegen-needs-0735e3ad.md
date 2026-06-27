---
title: GEMM codegen needs barrier-safe grid model (not the elementwise early-bounds-branch)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T19:05:49.127290+02:00"
---

Static invariant: a shared-memory kernel's bar.sync must be reached by ALL threads in the block (uniform control flow). FOUND 2026-06-27 wiring STAGE/SLOAD codegen: EMIT-GRID-CTX (lib/ptx/cg.f) emits an early '@%p1 bra DONE' bounds branch, so out-of-bounds lanes skip any subsequent bar.sync -> DEADLOCK. The elementwise (1 elem/thread, early-branch) emit model therefore CANNOT host the shared-staging barriers a tiled GEMM needs. FIX (part of habu-tiled-gemm-codegen): a 2-D cooperative-tiling emit model where all block threads participate in STAGE+bar.sync and bounds-masking is deferred to the final STORE (predicated), matching cg-matmul.f's hand-written structure. Until then STAGE/SLOAD bodies stay E-PTX-NOIMPL (typed, not emitted). This is why ACC codegen landed (no barrier) but STAGE/SLOAD did not. Files: lib/ptx/cg.f grid/barrier model. Dep: blocks the device-correct checked GEMM (habu-re-express-tiled d2).
