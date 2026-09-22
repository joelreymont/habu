---
title: Share one residency pass between the two selectors
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T06:42:51.079048+03:00"
---

Problem: after 768a563c X64SEL carries a verbatim copy of A64SEL's data-stack residency pass (select-x64.f 'which slot holds which value, over the whole routine', about 280 lines: D-IN/D-OUT/D-CUR/D-MEET/D-NEED, the DSAVE/DBACK/DEXIT/DCALL/DOP transfers, DRES-FIX, DNEED-FIX) and regalloc-verify.f holds the same CFG fixpoint a third time (VDRES-FIX). The copy was deliberate: the ARM64 chain requires select.f alone (src/arch/arm64/passes.f) and a shared module would have moved it in a lane that may not. Acceptance: one residency pass parameterised by the dialect, the way regalloc.f and regalloc-verify.f are one pass over a dialect vocabulary, both selectors calling it and the copied words deleted from select-x64.f; ARM64 census byte-identical; native-select, native-regalloc, native-tail, x64-select, x64-regalloc, x64-chain green; tools/compile-floor before and after (the pass is on the ARM64 compile path). Files: src/compiler/native/select.f, src/compiler/native/select-x64.f, a new src/compiler/native/residency.f, test/compiler/. Verify: the suites; census; compile-floor; three generations with cmp; test/run.f. Depends: none. Ownership: tier-1 selection (hazel). Claim: unassigned.
