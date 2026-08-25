---
title: If-convert a region whose blocks meet inside it
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T11:27:10.722515+02:00"
---

Problem: src/compiler/native/select.f admits an if-conversion only when every block of the region has exactly ONE predecessor inside it, because the pass emits each block's operations once and a block reached on two paths would arrive with two different sets of block arguments. That refuses a nested two-armed if, whose inner join is reached from both inner arms - `x if y 0 > if 1 else 2 then else 3 then` is the smallest example. Acceptance: a predicate per block and a select per block argument, so an inner join's arguments are chosen rather than handed over, with the pressure bound extended to count them; cases in test/compiler/native-select.f for the nested shape and for the shape that is still refused. Files: src/compiler/native/select.f, test/compiler/native-select.f. Verify: bin/hb --load test/compiler/native-select.f, native-chain.f, tools/codegen-compare.f. Depends: none. Ownership: the selector's if-conversion. Claim: unassigned.
