---
title: Fix payload suite linearity overclaims
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:07.329751+02:00"
closed-at: "2026-08-02T16:43:02.299750+02:00"
close-reason: authoritative ancestor 5b0ebb070a5b8ef7c04e2d28772421f796b686c6 deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host architecture and suites; retaining the task would resurrect deleted architecture.
---


gpt2-payload-test.f plus FILEMAP.md, design frozen. Add: the accepted ptr held identity row and accepted OVER and TUCK twins. Remove from linearity evidence: GP-CTOR-BRANCH, GP-CTOR-TWICE, GP-M-KEEP, GP-M-ESCAPE, and GP-M-EXIT — their non-linear forms are generically invalid, so they cannot discriminate. Any other unpaired candidate is retained ONLY after adding an accepted same-shape twin. FILEMAP enumerates the resulting exact pairs and claims nothing beyond them. Acceptance: suite rc=0 with the new controls certifying; every remaining linearity claim has a passing width-identical non-linear twin; diff lints clean.
