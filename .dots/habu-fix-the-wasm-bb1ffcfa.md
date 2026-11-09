---
title: Fix the Wasm tests and re-enable them
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T19:06:08.142522+02:00"
---

Joel 2026-11-09: "disable lsp and x86 and wasm. get arm-to-arm done first, including pg. get that fully working then tackle wasm and x86. do not delete tests unless they are absolutely not load-bearing, disable them. add ifdefs if we don't have those". Order: arm-to-arm working (pg included), then this dot, then habu-re-enable-the-959502b5. wasm-backend, wasm-capture and wasm-link-capture are disabled with `[if]` in the landing stack until this dot re-enables them.

Problem 1 (test/wasm/backend.f; test/wasm/capture.f's first failure): -8681 E-NSHADOW-ROW at src/compiler/native/shadow.f:493 PATCH-DOES. backend.f:140 `8 EMPTYD BK-EMPTY` patches record 33734 while the shadow holds 33727..33733: a word `create` defines at tier 0 is never filed in an open Wasm shadow. On master the kernel's habu2.f EMIT-CREATE called ADDR-SHADOW, (habu2.f:4627), which calls NCOMP:SHADOW-FIXED (compiler.f:791) at any tier. Now the startup runs RUNTIME:INSTALL (tools/hb-build-core.f:187), so `create` goes OUTER:INSTALL-CREATE -> definers.f DEF-CREATE-ROW -> DEF-ADDR-TAIL (definers.f:761) -> DEF-FIXED, and T0's FIXED-LOWER -> SHADOWED (t0.f:1861) returns early because T0 has no Wasm emitter (EMIT-PROVIDED?); nothing reads FIXED-SHADOW-CELL. A defect the stack introduced. Proof (RCA worker, 2026-11-09): an hb whose DEF-ADDR-TAIL reads the latch before DEF-CLOSE (which clears DEF-TIER-CELL) and runs `latch 2 = if DEF-HERE COMP-BODY:address ndict@ 1- NCOMP:SHADOW-FIXED then` after it passes backend.f (rc 0). Fix: file every fixed body T0 lowers (create, constant, defer, cast, as C-CONSTANT, C-IDENTITY-SHADOW and DEFER-SHADOW, do) in an open shadow T0 does not file itself (not native, no EMIT-PROVIDED?), in DEF-FIXED or T0 COMPILE-FIXED.

Problem 2 (test/wasm/link-capture.f; capture.f's second failure): REG-AOT-OVERFLOW at src/core/type-family.f:3566, REG-AOT-SAVE's per-store check, against AOT-REG-CAP $20000 (src/habu/aot-decl.f:548, as on master). The window adds nothing (mark = end in every store), so each capture carries the whole registry: families 202 x 176, parameter kinds 114 x 8, sum variants 540 x 80, fields 193 x 88, type names 12471 bytes, schema nodes 758 x 32; store 6 starts at byte 109319 and needs 24256 against 21753 left. twzrnlzk widened TF-REC from 21 to 22 cells (+1616 bytes); at 21 cells the total is still 131959, so the counts grew too. Open: master's counts at the same capture, from a registry-count probe in a master-built hb; until then whether the stack broke it is unknown.

Shadow, compiler and type-family sources are built into hb, so `require` skips them and a source probe needs a rebuilt hb (tools/hb-build.f --engine, about 10 s).

Acceptance: the three tests re-enabled and passing; each fix in place of the defect, not around it. Files: src/habu/definers.f, src/compiler/t0.f, src/core/type-family.f, test/gate-stdlib-cases.f. Verify: bin/hb --load test/run.f -- wasm-backend wasm-capture wasm-link-capture on an hb built from the fixed tree. Depends: arm-to-arm landed with pg working. Ownership: those files. Claim: unassigned.
