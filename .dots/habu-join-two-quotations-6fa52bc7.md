---
title: Join two quotations whose regions are minted per execution
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T04:28:08.921881+02:00"
---

Problem: `: P-JOIN ( bool -- ) IF [: RR-OWN ;] ELSE [: RR-OWN ;] THEN execute drop ;` (after `defer RR-OWN ( -- matrix<space-global,fresh-region-a,f32,f32> )`) is refused with a region mismatch at THEN on master d411bc5b: the two literals carry different rigid atoms. Under the installed hb, `hb --load` of those two lines prints `habu: in p-join: at 'THEN' rigid host: region mismatch (different allocation) expected: [ -- matrix<space-global,fresh-region-a-b,f32,f32> ] actual: [ -- matrix<space-global,fresh-region-a-a,f32,f32> ]` and exits 70. Once lane qmint (habu-mint-a-quotation-060811e6) mints a quotation's fresh regions per execution, the two arms' atoms are both per-execution and the join is sound. Acceptance: P-JOIN admitted; a join that mixes a per-execution atom with a fixed one stays refused; a row in test/rigid-region-suite.f. Files: src/core/checker.f (the join's unification of quotation types). Verify: row rigid-region. Depends: habu-mint-a-quotation-060811e6. Claim: unassigned.
