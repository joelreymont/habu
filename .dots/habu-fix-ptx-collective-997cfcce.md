---
title: Fix PTX collective mask and block semantics
status: active
priority: 1
issue-type: task
created-at: "2026-06-27T15:32:50.883376+02:00"
---

Deep-review finding 2026-06-27: lib/ptx/cg-collective.f ROW-LOAD seeds inactive lanes to -inf and erases the mask, which is valid for BLOCK-MAX but wrong for BLOCK-SUM and backward loads; collective codegen also hardcodes SM-BLK=256/SMEM[1024] while some fixtures/docs use block-1024; WHERE constraints are skipped instead of validated. Correct fix: carry lane predicate/mask in emitted tile representation or make each collective apply its own identity, derive/reject block size consistently from PTX-BLOCK@, parse WHERE enough to validate launch-time constraints, and add device tests for direct row sum, softmax/backward with k < block, malformed WHERE, block mismatch, and k > block.

Mac proof 2026-06-29: codegen already applies reducer-local identities in EMIT-REDUCE and derives SMEM/fold bounds from PTX-BLOCK@. Added tools/ptx/sum1024-cg.f plus saxpy-test assertions for SUM_ROWS_1024: SMEM[4096], zero inactive identity, setp.ge bound 1024, and add.f32 reducer. Focused PTX text fixture and filemap-lint pass on macOS.

Remaining zed proof: run CUDA device goldens for direct row sum, softmax forward, softmax backward/gradcheck with k < block, plus launch-fail checks for malformed WHERE/block mismatch/k > block. Close only after those device paths fail closed and pass on Orin.
