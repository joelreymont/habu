---
title: "Mint a declared quotation input's fresh region per execution"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T04:49:53.093136+02:00"
---

Problem: a callee that declares a quotation input whose output carries a fresh region names that region once per call, so executing the quotation twice in its body yields one region twice. Admitted on master and on lane qmint's candidate: `: K-QOUT-BODY ( [ -- matrix<space-global,fresh-region-b,f32,f32> ] -- ) dup execute swap execute RR-UONE ;` (/tmp/jerry-fcall-tmp/p1.f line 43; answers p1-before.txt line 58). It is the body-side twin of habu-mint-a-quotation-060811e6: no caller can pass such a quotation since that lane, so today the body is unreachable from checked callers, but the body itself is admitted. Acceptance: the checker refuses a body that executes a declared fresh-output quotation input more than once with region mismatch, or a design lets the declared type say it runs once; a rigid-region row shows the refusal (fails on master). Design first (lead, then Joel if a new declaration form is needed: a 'run once' quotation type). Files: src/core/checker.f (QUOT-APPLY-INST, fresh-name minting of a callee's quotation input), test/rigid-region-suite.f, docs/forth.md 'Rigid host-allocation identity domains'. Verify: rows rigid-region, linear. Depends: lane qmint landed (habu-mint-a-quotation-060811e6). Claim: unassigned.

The declared-defer twin is admitted too: the qmint worker's case I10, a defer declared `( -- [ -- M<fresh> ] )` whose quotation the body executes twice, passes on master, on lane qmint and on its fold 1 (probe /tmp/jerry-qmint-tmp/f2/inv.f).
