---
title: "Checker capability: typed kernel loops + shared-mem tiles + accumulators"
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T16:52:05.237047+02:00"
---

THE central gap: the headline compute kernels (lib/ptx/cg-matmul.f tiled SGEMM, lib/ptx/cg-attention.f fused attention) are currently UNCHECKED raw-PTX emit boundaries, because the tile DSL type system (lib/ptx/tile.f) cannot express (a) a CHECKED COUNTED LOOP (the K-loop / streaming reduction - body stack effect must be loop-invariant, induction var typed), (b) a SHARED-MEMORY TILE TYPE (matrix tile staged to .shared, distinct address space from global span), and (c) a REGISTER ACCUMULATOR type threaded across the loop. Until these land, our flagship 'checked target beats Triton' kernels are documented boundaries, not checked - undercutting the thesis exactly where it matters most. FIX: extend the checker (src/core/checker.f) + the tile type system with: a checked-loop combinator typing ( acc -- acc ) bodies; a shared<t,r,c> tile type + STAGE/SMEM-LOAD ops with the address-space rule (space-shared vs space-global never unify); an accumulator tile type. Then re-express EMIT-MATMUL / EMIT-ATTN as CHECKED KERNEL: bodies composed from typed words (like SAXPY/softmax) and DELETE the unchecked boundary. Needs checker.f + a fixpoint rebuild (recovery via a known-good bin/hb). VERIFY: a tiled GEMM written in the checked DSL certifies, emits the same PTX, stays device-correct; the unchecked-boundary note is removed from cg-matmul.f/cg-attention.f. Deps: relates to habu-tiled-gemm-codegen (perf) and habu-ptx-m11-attention (kernel), but this is the TYPE-SYSTEM capability, not codegen.
