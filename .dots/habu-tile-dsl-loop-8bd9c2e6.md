---
title: Tile-DSL loop body cannot capture enclosing locals (blocks GEMM re-express)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T18:33:19.054874+02:00"
---

Static invariant: a TILE-LOOP/ACC-LOOP body quotation [: ... ;] must be able to reference the kernel's enclosing locals (the A/B spans, gridctx, and the loop index k) so each K-step can load the k-th operand tiles. FOUND 2026-06-27 probing capability (c): a body like 4 swap [: a g LOAD b g LOAD ACC-FMA ;] ACC-LOOP fails to check (cryptic 'a' diagnostic) - the locals a/b/g are not visible inside the [: ;] quotation. WHY IT MATTERS: without local capture (or an index-passing loop variant), the re-expressed checked GEMM K-loop cannot compute per-k offsets, blocking habu-re-express-tiled (d). FIX OPTIONS: (1) make tile-DSL quotations close over enclosing locals; (2) add an index-passing loop ACC-LOOP-I ( n acc [ n acc -- acc ] -- acc ) that threads k + lets the body take the spans on the stack. Decide + implement + checked positive/negative tests. Files: lib/ptx/tile-loop.f / tile-acc.f + checker quotation handling if needed. Dep: blocks habu-re-express-tiled; relates to habu-checker-capability-typed.
