---
title: Write tier 0 as typed Habu
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T22:57:08.834242+02:00"
---

Rule: docs/architecture.md:144-147, hand-written code per platform is only the kernel's primitives. Problem: tier 0 is a second ARM64 codegen written as encoder rows outside src/compiler: src/habu/jit.f (1001 lines; header 5-8 'a second ARM64 codegen beside the platform codegen'; EMIT-VLITPUSH, EMIT-VSPILL, EMIT-VPUSHC, FOLD-ENTRY write runtime routines as A64ASM rows) and the tier-0 handlers in src/habu/habu2.f (J-DOES ~4593, C-LBRACE ~6202, EM-COMPILE-LEGACY ~13262; about 360 C-/J-/EMIT- words). Acceptance: tier 0 is typed Habu under src/compiler/native sharing tier 1's encoders; the only hand-written ARM64 left is kernel primitives; --load and the REPL keep tier 0's compile cost (tools/compile-floor.f before/after). Depends: the tier ruling in habu-decide-whether-one-358fc93b (if one pass replaces tier 1 or tier 0, this dot changes or goes). Files: src/habu/jit.f, src/habu/habu2.f, src/compiler/native. Verify: test/run.f, two-generation build, compile-floor and tier-bench medians.
