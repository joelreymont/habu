---
title: Join two quotations whose regions are minted per execution
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T04:28:08.921881+02:00"
---

Problem: `: P-JOIN ( flag -- ) IF [: RR-OWN ;] ELSE [: RR-OWN ;] THEN execute drop ;` (RR-OWN ( -- matrix<space-global,fresh-region-a,f32,f32> )) is refused with a region mismatch at THEN on master d411bc5b (/tmp/jerry-qmint-tmp/q1.f P-JOIN): the two literals carry different rigid atoms. Once lane qmint (habu-mint-a-quotation-060811e6) mints a quotation's fresh regions per execution, the two arms' atoms are both per-execution and the join is sound. Acceptance: P-JOIN admitted; a join that mixes a per-execution atom with a fixed one stays refused; a row in test/rigid-region-suite.f. Files: src/core/checker.f (the join's unification of quotation types). Verify: row rigid-region. Depends: habu-mint-a-quotation-060811e6. Claim: unassigned.
