---
title: Fuse boolean results into branches at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:09:22.609973+03:00"
---

Problem: tier 1 materializes every boolean: `0=` compiles as `mov x1,#0; cmp x0,x1; cset x0,eq; neg x0,x0` and a following `0=` repeats it, so `REQUIRE-BOOT-V @ 0= 0=` spends 8 instructions producing a flag it could load in 1 (measured 2026-09-16, REQUIRE-BOOT-OPEN?); a boolean consumed by `if` is materialized, stored to the stack, reloaded and tested with cbz instead of one conditional branch (REQUIRE-BOOT-LIMIT). Acceptance: tier 1 selection folds double negation, keeps a comparison result in flags when its only consumer is a branch (cmp + b.cond, or cbz/cbnz on the loaded value), and materializes a boolean only when it escapes to the stack or a store; the sample words shrink to their loads and one branch; a fixture pins the instruction count of five representative shapes (0=, 0= 0=, = if, < if, and-of-two-tests); baked code bytes before and after; byte fixpoint; full gate green. Files: src/compiler/native/select.f, combine.f, a64ir.f, test/compiler/*.f. Verify: tools/jitdump.f on the shapes; test/compiler suites with the test/compiler/aot-mode.f prefix; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: tier-1 selection. Claim: unassigned.
