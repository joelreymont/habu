---
title: Convert the raw pointer cells in src/compiler and src/arch
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T18:44:23.881790+03:00"
---

Problem: the raw-storage census (/home/joel/.cache/hazel/scout-pun/census.md) lists the compiler sites the rule habu-refuse-a-ptr-5ad2734e refuses: src/compiler/native/regalloc.f (43), regalloc-verify.f (24), select.f (23), elaborate.f (17) and the rest under src/compiler and src/arch. Acceptance: every mechanical-shape site converted to the declared form the census names; the compiler suites green on the release engine and each converted file's closure loading on ~/.cache/hazel/engines/raw-rule-gen1; byte fixpoint (the compiler is baked); test/run.f. Files: src/compiler/, src/arch/ per the census. Verify: test/compiler suites; fixpoint; test/run.f. Depends: habu-parameterise-the-alloc-7efbe7a1 landing first (it rewrites regalloc.f's register constants and regalloc-verify.f), habu-fix-the-definers. Ownership: compiler. Parent: habu-refuse-a-ptr-5ad2734e. Claim: agent=hazel-raw-compiler workspace=.jj-ws/hazel-raw-compiler.
