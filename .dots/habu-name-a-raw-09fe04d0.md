---
title: Name a raw-cell refusal that lands on the return row
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T12:50:13.891590+03:00"
---

Problem (adversarial review of the 2026-09-18 batch, differential probe against the pre-batch engine raw-rule-gen3): deleting RAW-PTR-DIAG-CLASSIFY in bb13bc30 lost the name of a raw-cell pointer refusal captured on the RETURN-stack row — 'variable RV  RS3 ( | -- | ptr n ) RV @ >r' answered E-RAW-CELL-PTR / declare_pointer_cell with the rule's reason before and answers E-REJECTED / fix_return_stack with no reason now; RSUNI / RSUNI-IN (src/core/checker.f ~11402) unify the return row without UF-CAPTURE, so neither RAW-PTR-HIT nor BASE-PTR-HIT is latched there; the base rule has the same blind spot ('B1 ( | -- | bthing ) data-base 8 + @ >r', 'B2 ( | -- | ptr n ) …' refuse unnamed) while the data-row, locals-annotation and quotation-row equivalents are named. Of nine raw-rule shapes run through both engines only the return-row one diverged. Acceptance: the return-row unifies capture through the same first-failure path (UF-CAPTURE or a return-row twin that latches the pending hit flags while the pin is open), so the two probes and their base-rule twins name E-RAW-CELL-PTR with the rule's reason; cases in test/compiler/raw-cell-pointer-refusals.f and base-pointer-arith-refusals.f red-first; the golden diagnostics unchanged (report any that move). Files: src/core/checker.f, the two fixtures. Verify: the fixtures; the diagnostic suites; three generations with cmp; test/run.f. Depends: none. Ownership: checker diagnostics. Claim: agent=hazel-return-row workspace=.jj-ws/hazel-return-row.
