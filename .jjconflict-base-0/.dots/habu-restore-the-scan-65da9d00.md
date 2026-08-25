---
title: "Restore the scan row's calling shape"
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T21:49:21.006610+02:00"
---

tools/codegen-workload-hot.f SCAN-BODY$: the scan workload models the checker's symbol comparison - a fold called once per byte from the comparison's own loop - and its two arms stopped being the same shape when the chain's FOLD-C fell to exactly 40 bytes (habu-place-the-data-9f128e58 removed its two data-stack pointer moves). The engine copies a bare record of 40 bytes or fewer (INL-MAX in src/habu/habu2.f), so the arm compiled over the chain's column now holds the fold's body instead of a call to it, while the old arm still calls. The row's delta therefore reports more than migrating the word buys the real checker, whose callers were compiled before the migration and keep their call - src/compiler/native/reach.f redirects those sites, it does not recompile the callers. Decide and implement: either force the scan driver to a calling shape by construction (so the row keeps modelling what it names, and stops depending on which side of the engine's inline limit the subject happens to land), or split the row into a called row and a copied row and say what each measures. Do not tune the subject's size: it is a real checker word. The wiring proof is already sound either way - tools/codegen-workload-scan.f COPIED-FROM? reads the chain's own body out of the arm - so what is at stake is what the delta MEANS, not whether the arm is wired right. Acceptance: tools/codegen-workload-test.f green with the row's claim and its prose agreeing, and the head of tools/codegen-workload-hot.f no longer carrying the interim paragraph.
