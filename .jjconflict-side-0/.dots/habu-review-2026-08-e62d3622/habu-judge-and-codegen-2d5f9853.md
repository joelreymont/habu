---
title: judge and codegen-compare are two harnesses for one comparison
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.192026+02:00"
---

Problem: tools/judge.f:14-20 says codegen-compare is 'now deleted' and two lines later that both run side by side; tools/codegen-compare-*.f = 15 files / 3,313 lines with one gate reference; tools/judge/ = 5,699 lines plus judge-test.f, judge-fuzz*.f, judge-timed.f (hand-run); tools/judge/fuzz.f:315 and codegen-compare-corpus2.f:189,220, corpus3.f:72-75,190-191 carry commented-out definitions. Acceptance: codegen-compare deleted with its gate row once the judge covers its corpus (state what it does not); commented-out code removed; the judge header corrected. Files: tools/codegen-compare-*.f, tools/judge*. Verify: judge --check; gate. Depends: habu-judge-gate-is-8a2af19b. Ownership: judge. Claim: unassigned.
