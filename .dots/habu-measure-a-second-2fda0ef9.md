---
title: Measure a second does> clause re-patching one created record
status: open
priority: 3
issue-type: task
created-at: "2026-09-19T18:45:43.515074+03:00"
---

Problem (adversarial review of fba695f8..39d9a399, unresolved): EM-REC-WIDE-PUBLISH's min-in poke is an ORR (src/habu/habu2.f ~2798) and BWIDEMARK only sets, so on the does> publish tail - which targets an EXISTING record (ndict-1), unlike the colon/cast/defer tails whose record is fresh - a second does> clause re-patching the same created word (a definer whose body calls another definer and then declares its own does>) would OR the second clause's min-in bits over the first's and a first-clause DNAME-WIDE would stick. No front-end rule refusing a does> without a create in the same body was found (J-DOES, CHECK-DOES!, elaborate.f), and whether the pattern is admitted was not confirmed. Acceptance: measure on the engine whether ': A: ( -- ) B: ... does> ( ... ) ... ;' (a body that creates through another definer and then patches) is admitted; if admitted, the publish tail must reset the record's wide/min-in facts before applying the new clause's (or the pattern is refused by name at does>); a red-first fixture in test/runtime-regression-test.f either way; docs/forth.md's does> paragraph states the rule. Files: src/habu/habu2.f, bootstrap/cg/forth.fs (mirror), test/runtime-regression-test.f, docs/forth.md. Verify: the fixture; three generations with cmp; bootstrap check; test/run.f. Depends: none. Ownership: engine publication. Claim: unassigned.
