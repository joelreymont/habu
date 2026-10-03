---
title: Require what aot-file.f, aot-lib.f and icode.f use
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T18:24:49.016990+02:00"
---

Problem: same class as dot cc69dbbe (r4-acapreq). src/habu/aot-file.f:149-150 'using AOT-BUF' and 'using AOT-WINDOW' (aot-decl.f packages) with no require: alone rc 91, and src/habu/aot-owned.f fails the same way through its require of aot-file.f (:5). src/habu/aot-lib.f alone rc 70 E-UNDEFINED: XDS (src/arch/arm64/mnem.f / machine.f), its header saying 'Load after src/habu/aot-closure.f' instead of requiring. src/arch/arm64/icode.f:7 imports A64ASM without requiring asm.f and loads alone only because the engine keeps that package. Found by review 213 ($HOME/.cache/tmp/kestrel-r4-rev213/probes.log). Acceptance: each file requires its own dependencies (card section 7); tools/standalone-load-test.f rows for aot-file.f, aot-owned.f and aot-lib.f fail first and then pass; a require of a file inlined into the engine text is a no-op there (a provided row in tools/bootstrap.sh's inlined text and a BF-APPEND-MODULE or boot row in tools/build-fixpoint.f, as cc69dbbe did), so g1 = g2 with .names and the engine is byte-identical to the parent's; two-generation build; build-fixpoint stdin --force; Gforth check-only recovery once on the final tree. Base: after r4-acapreq (cc69dbbe). Files: src/habu/aot-file.f, src/habu/aot-lib.f, src/arch/arm64/icode.f, tools/standalone-load-test.f, tools/build-fixpoint.f if a row is missing.
