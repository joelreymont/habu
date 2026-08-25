---
title: Drop a select nothing reads and share one compare
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T11:27:10.731672+02:00"
---

Problem: src/compiler/native/select.f builds one select per position of the join, and one fused compare per select. Both are wasteful where the source does not read the answer: `2dup < if swap then drop` hands the join two values and drops one, so the chain emits two Cmp and two Csel where one of each would do - visible in test/compiler/native-chain.f BRANCH-CASE, which asserts two of each today. Acceptance: a select whose result no later operation reads is not built, and two selects in one block under the same comparison of the same two values share one Cmp; the chain case's counts drop and its answers do not move. Files: src/compiler/native/select.f, test/compiler/native-chain.f. Verify: bin/hb --load test/compiler/native-chain.f, native-select.f, tools/codegen-compare.f. Depends: none. Ownership: the selector's if-conversion. Claim: unassigned.
