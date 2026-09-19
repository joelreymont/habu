---
title: Measure a second does> clause re-patching one created record
status: active
priority: 3
issue-type: task
created-at: "2026-09-19T18:45:43.515074+03:00"
---

Claim: alder, .jj-ws/alder-does-repatch, base b4efad25.

Measured: nested defining words are admitted at both tiers. Replacing an inner
one-input clause with a zero-input clause leaves an underdepth guard; replacing
a wide clause with a scalar clause leaves the wide guard. The runtime fixture
also pins min-in 2 -> 1, which previously ORed to 3 instead of replacing it.

Fix: after recording the replacement signature, clear exactly min-in and wide
on the created record, then run the existing publication tail. An empty outer
clause must also undo the previous branch: otherwise clearing the old wide
guard exposes the still-running wide body. The common patch path writes RET
for an empty replacement, preserving intact kind stamps and leaving an already
cleared kind conservative. A fresh empty clause already ending RET avoids the
write/protection path. The stage0 mirror follows the same rule.

The old product fails the new runtime fixture with underdepth on GE-A. Rebuilt
product passes the entire runtime row, including both tiers and wide -> empty
pointer storage. Existing does-clause-record, does-empty-clause, underdepth,
all six tools source-reader rows and friend-arena-absence pass; bootstrap
codegen also passes standalone and in the full eleven-file tail-pure-fixtures
row. Check-only Gforth bootstrap passes. Astra's empty-clause and tier-setup
findings are fixed; focused re-review is clear. Three private product
generations are byte-identical (gen2 == gen3). Artifacts and logs are under
/tmp/alder-does-repatch; Hazel owns the full integration gate.

Problem (adversarial review of fba695f8..39d9a399, unresolved): EM-REC-WIDE-PUBLISH's min-in poke is an ORR (src/habu/habu2.f ~2798) and BWIDEMARK only sets, so on the does> publish tail - which targets an EXISTING record (ndict-1), unlike the colon/cast/defer tails whose record is fresh - a second does> clause re-patching the same created word (a definer whose body calls another definer and then declares its own does>) would OR the second clause's min-in bits over the first's and a first-clause DNAME-WIDE would stick. No front-end rule refusing a does> without a create in the same body was found (J-DOES, CHECK-DOES!, elaborate.f), and whether the pattern is admitted was not confirmed. Acceptance: measure on the engine whether ': A: ( -- ) B: ... does> ( ... ) ... ;' (a body that creates through another definer and then patches) is admitted; if admitted, the publish tail must reset the record's wide/min-in facts before applying the new clause's (or the pattern is refused by name at does>); a red-first fixture in test/runtime-regression-test.f either way; docs/forth.md's does> paragraph states the rule. Files: src/habu/habu2.f, bootstrap/cg/forth.fs (mirror), test/runtime-regression-test.f, docs/forth.md. Verify: the fixture; three generations with cmp; bootstrap check; test/run.f. Depends: none. Ownership: engine publication. Claim: unassigned.
