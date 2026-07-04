---
title: "Maki: fold staged transpose into consumer index math"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:50:03.555368+02:00"
---

SLICE 4 folded MVV-FREE movement (reshape/slice) into EW/RED/MM load base pointers (maki/move-view.f MVW-RESOLVE-OFF; lower-ew/red/mm LEW/LRED/LMM-APPLY-VIEWS), but MVV-STAGED (transpose) dissolved into a compute region fails closed E-MVW-STAGED (maki/move-view.f:MVW-OFF-ELEMS). A transpose is a lane permutation dst[i,j]=src[j,i], not a constant base offset, so folding it needs a per-element remapped load (compute i=e/dst_cols, j=e mod dst_cols, src=j*src_cols+i in the kernel body) - the same permutation lower-move.f LMV-BODY-TRANSPOSE already emits for the materialized copy. Fix: extend the EW flat-index/RED row/MM K-loop loaders to accept a per-input transpose remap (expose the flat index reg from EMIT-GRID-CTX), or have fusion-plan.f materialize a transpose prologue instead of dissolving it. Device-proven cases today: SLICE into EW/RED/MM. Reproduce: MODEL: TG ( x:4x8 -- y ) TRANSPOSE GELU ; FP-BUILD 0 LEW-ANALYZE -> E-MVW-STAGED (maki/lower-mv-test.f).
