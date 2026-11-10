---
title: "Wasm 2b: T0's Wasm generator, control flow"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T10:14:00.375982+02:00"
---

Step 2 of the Wasm campaign, second lane. Problem: src/habu/t0-wasm.f fills only the straight-line cells of the tier 0 seam, so a definition with if, begin or case compiled for Wasm ends hb with 'hb: unset quotation' (exit 86, src/habu/t0-emit.f :500-511). Acceptance: t0-wasm.f fills the control cells (IF, ELSE, THEN, BEGIN, WHILE, REPEAT, UNTIL, the -CC forms, AGAIN, CASE, OF, OF-LIT, ENDOF, ENDCASE) in one pass: every open source structure is one open Wasm block on a private stack, a branch names its target by depth, and a while is an if nested in the loop. DO loops, leave, the return stack, quotations, recurse and the callback stub stay unset. Files: src/habu/t0-wasm.f. Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w2b.md (the engine builds; the straight-line check extended with a word per control form validates under wasm-tools and bun runs it to status 0, and a wrong expected value makes it exit 2). Depends: habu-wasm-2a-t0-601842d4 (merged in the integration workspace). Ownership: src/habu/t0-wasm.f. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.

Merged into the integration workspace as kyltpqtx (not landed). Evidence: the engine builds in 13 s; the check (48 functions, 8338 bytes) validates and runs to status 0, and a wrong expected value exits 2. Not reached: two whiles in one begin loop, which the checker refuses (E-REJECTED at the second while), so no source can ask for it.
